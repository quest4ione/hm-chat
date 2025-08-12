use std::{
    collections::HashMap,
    fmt::Display,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use reqwest::{Method, StatusCode};
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    Api(#[from] ApiError),
    Reqwest(#[from] reqwest::Error),
    Json(#[from] serde_json::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Api(e) => write!(f, "ApiError: {e}"),
            Self::Reqwest(e) => write!(f, "ReqwestError: {e}"),
            Self::Json(e) => write!(f, "JsonError: {e}"),
        }
    }
}

#[derive(Error, Debug)]
pub enum ApiError {
    #[error("invalid value for field {field}:  {msg}")]
    InvalidValue {
        field: String,
        msg: String,
    },

    #[error("rate limit exceeded: {count} per {}", duration.as_millis())]
    RateLimited {
        count: u8,
        duration: Duration,
    },

    #[error("invalid or expired token")]
    Unauthorized,

    #[error("unknown error({code}): {}", msg.clone().unwrap_or("".to_string()))]
    Unknown {
        code: StatusCode,
        msg: Option<String>,
    },
}

#[derive(Debug)]
enum RawResponse<T> {
    Success(T),
    Failure(String),
}

impl<'de, T> Deserialize<'de> for RawResponse<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        struct Helper {
            ok: bool,
            #[serde(flatten)]
            data: serde_json::Map<String, serde_json::Value>,
        }

        let raw = Helper::deserialize(deserializer)?;

        if raw.ok {
            T::deserialize(raw.data)
                .map(Self::Success)
                .map_err(serde::de::Error::custom) // TODO: this can probably be better but types hurt my brain
        } else {
            #[derive(Deserialize)]
            struct ErrorMsg {
                msg: String,
            }

            ErrorMsg::deserialize(raw.data)
                .map(|err| Self::Failure(err.msg))
                .map_err(serde::de::Error::custom) // TODO: this can probably be better but types hurt my brain
        }
    }
}

pub trait Endpoint: Serialize {
    const URL: &'static str;
    const METHOD: Method;

    type Response: DeserializeOwned;

    fn create_request(&self, client: &reqwest::Client) -> Result<reqwest::Request, Error> {
        Ok(client
            .request(Self::METHOD, Self::URL)
            .header(reqwest::header::CONTENT_TYPE, "application/json")
            .body(serde_json::to_string(self)?)
            .build()?)
    }

    #[allow(async_fn_in_trait)]
    async fn deserialize_response(res: reqwest::Response) -> Result<Self::Response, Error> {
        let code = res.status();
        let body = res.text().await?;

        let raw_response: serde_json::Result<RawResponse<Self::Response>> =
            serde_json::from_str(&body);

        match raw_response {
            Ok(RawResponse::Success(val)) if code.is_success() => Ok(val),
            Ok(RawResponse::Success(_)) => Err(Error::Api(Self::map_error(code, None))),
            Ok(RawResponse::Failure(msg)) => Err(Error::Api(Self::map_error(code, Some(msg)))),
            Err(e) => Err(Error::Api(Self::map_error(code, Some(e.to_string())))),
        }
    }

    #[allow(async_fn_in_trait)]
    async fn send(&self, client: &reqwest::Client) -> Result<Self::Response, Error> {
        let req = self.create_request(client)?;
        let res = client.execute(req).await?;

        Self::deserialize_response(res).await
    }

    fn map_error(code: StatusCode, msg: Option<String>) -> ApiError;
}

#[derive(Serialize, Debug)]
pub struct GetTokenRequest {
    pass: String,
}

impl GetTokenRequest {
    pub fn new(pass: String) -> Self {
        Self { pass }
    }
}

#[derive(Deserialize, Debug)]
pub struct GetTokenResponse {
    #[serde(rename = "chat_token")]
    pub token: String,
}

impl Endpoint for GetTokenRequest {
    const URL: &'static str = "https://www.hackmud.com/mobile/get_token.json";
    const METHOD: Method = Method::POST;

    type Response = GetTokenResponse;

    fn map_error(code: StatusCode, msg: Option<String>) -> ApiError {
        match (code, msg) {
            (StatusCode::UNAUTHORIZED, Some(msg)) if msg == "expired or invalid pass" => {
                ApiError::InvalidValue {
                    field: "pass".to_string(),
                    msg,
                }
            }
            (code, msg) => ApiError::Unknown { code, msg },
        }
    }
}

#[derive(Serialize, Debug)]
pub struct AccountDataRequest {
    #[serde(rename = "chat_token")]
    token: String,
}

impl AccountDataRequest {
    pub fn new(token: String) -> Self {
        Self { token }
    }
}

#[derive(Deserialize, Debug)]
pub struct AccountDataResponse {
    pub users: HashMap<String, HashMap<String, Vec<String>>>,
}

impl Endpoint for AccountDataRequest {
    const URL: &'static str = "https://www.hackmud.com/mobile/account_data.json";
    const METHOD: Method = Method::POST;

    type Response = AccountDataResponse;

    fn map_error(code: StatusCode, msg: Option<String>) -> ApiError {
        match (code, msg) {
            (StatusCode::TOO_MANY_REQUESTS, Some(msg))
                if msg == "token pollrate exceeded - 1 per 4500ms" =>
            {
                ApiError::RateLimited {
                    count: 1,
                    duration: Duration::from_secs(5),
                }
            }
            (StatusCode::UNAUTHORIZED, None) => ApiError::Unauthorized,
            (code, msg) => ApiError::Unknown { code, msg },
        }
    }
}

#[derive(Serialize, Debug)]
pub struct ChatsRequest {
    #[serde(rename = "chat_token")]
    token: String,
    usernames: Vec<String>,
    before: Option<f64>,
    after: Option<f64>,
}

impl ChatsRequest {
    pub fn before(token: String, usernames: Vec<String>, before: SystemTime) -> Self {
        Self {
            token,
            usernames,
            before: Some(before.duration_since(UNIX_EPOCH).unwrap().as_secs_f64()),
            after: None,
        }
    }

    pub fn after(token: String, usernames: Vec<String>, after: SystemTime) -> Self {
        Self {
            token,
            usernames,
            before: None,
            after: Some(after.duration_since(UNIX_EPOCH).unwrap().as_secs_f64()),
        }
    }

    pub fn latest(token: String, usernames: Vec<String>) -> Self {
        let after = SystemTime::now() - Duration::from_secs(10 * 60);
        Self::after(token, usernames, after)
    }
}

#[derive(Deserialize, Debug)]
pub struct ChatsResponse {
    pub chats: HashMap<String, Vec<Message>>,
}

impl Endpoint for ChatsRequest {
    const URL: &'static str = "https://www.hackmud.com/mobile/chats.json";
    const METHOD: Method = Method::POST;

    type Response = ChatsResponse;

    fn map_error(code: StatusCode, msg: Option<String>) -> ApiError {
        match (code, msg) {
            // this shouldnt happen as there's always exactly one of before/after
            // unless the server checks more than that, which i think it doesnt...
            (StatusCode::FORBIDDEN, Some(msg))
                if msg == "no valid before or after timestamp was provided" =>
            {
                ApiError::InvalidValue {
                    field: "before | after".to_string(),
                    msg,
                }
            }
            (StatusCode::FORBIDDEN, Some(msg)) if msg == "no valid usernames were provided" => {
                ApiError::InvalidValue {
                    field: "usernames".to_string(),
                    msg,
                }
            }
            (StatusCode::TOO_MANY_REQUESTS, Some(msg))
                if msg == "token pollrate exceeded - 1 per 700ms" =>
            {
                ApiError::RateLimited {
                    count: 1,
                    duration: Duration::from_millis(700),
                }
            }
            (StatusCode::UNAUTHORIZED, None) => ApiError::Unauthorized,
            (code, msg) => ApiError::Unknown { code, msg },
        }
    }
}

#[derive(Debug)]
pub struct Message {
    pub id: String,
    pub t: f64,
    pub from_user: String,
    pub msg: String,
    pub message_type: MessageType,
}

#[derive(Debug)]
pub enum MessageType {
    Tell,
    Send {
        channel: String,
        action: Option<ChannelAction>,
    },
}

#[derive(Debug)]
pub enum ChannelAction {
    Join,
    Leave,
}

impl<'de> Deserialize<'de> for Message {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        pub struct Helper {
            pub id: String,
            pub t: f64,
            pub channel: Option<String>,
            pub from_user: String,
            pub msg: String,
            #[serde(default)]
            pub is_join: bool,
            #[serde(default)]
            pub is_leave: bool,
        }

        let raw = Helper::deserialize(deserializer)?;

        if let Some(channel) = raw.channel {
            let action = match (raw.is_join, raw.is_leave) {
                (true, _) => Some(ChannelAction::Join),
                (_, true) => Some(ChannelAction::Leave),
                _ => None,
            };
            Ok(Self {
                id: raw.id,
                t: raw.t,
                from_user: raw.from_user,
                message_type: MessageType::Send { channel, action },
                msg: raw.msg,
            })
        } else {
            Ok(Self {
                id: raw.id,
                t: raw.t,
                from_user: raw.from_user,
                message_type: MessageType::Tell,
                msg: raw.msg,
            })
        }
    }
}

#[derive(Serialize, Debug)]
pub struct CreateChatRequest {
    #[serde(rename = "chat_token")]
    token: String,
    #[serde(rename = "username")]
    from: String,
    channel: Option<String>,
    #[serde(rename = "tell")]
    to: Option<String>,
    msg: String,
}

impl CreateChatRequest {
    pub fn send(token: String, from: String, channel: String, msg: String) -> Self {
        Self {
            token,
            from,
            channel: Some(channel),
            to: None,
            msg,
        }
    }

    pub fn tell(token: String, from: String, to: String, msg: String) -> Self {
        Self {
            token,
            from,
            channel: None,
            to: Some(to),
            msg,
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct CreateChatResponse {}

impl Endpoint for CreateChatRequest {
    const URL: &'static str = "https://www.hackmud.com/mobile/create_chat.json";
    const METHOD: Method = Method::POST;

    type Response = CreateChatResponse;

    fn map_error(code: StatusCode, msg: Option<String>) -> ApiError {
        match (code, msg) {
            (StatusCode::FORBIDDEN, Some(msg)) if msg == "sending messages too fast" => {
                ApiError::RateLimited {
                    count: 5,
                    duration: Duration::from_secs(20),
                }
            }
            (StatusCode::FORBIDDEN, Some(msg)) if msg == "channel doesn't exist" => {
                ApiError::InvalidValue {
                    field: "channel".to_string(),
                    msg,
                }
            }
            (StatusCode::FORBIDDEN, Some(msg)) if msg == "user doesn't exist" => {
                ApiError::InvalidValue {
                    field: "from".to_string(),
                    msg,
                }
            }
            (StatusCode::FORBIDDEN, Some(msg)) if msg == "tell user doesn't exist" => {
                ApiError::InvalidValue {
                    field: "to".to_string(),
                    msg,
                }
            }
            (StatusCode::FORBIDDEN, Some(msg))
                if msg == "msg too long or too many newlines (1000/10)" =>
            {
                ApiError::InvalidValue {
                    field: "msg".to_string(),
                    msg,
                }
            }
            (StatusCode::FORBIDDEN, Some(msg)) if msg == "must provide a msg" => {
                ApiError::InvalidValue {
                    field: "msg".to_string(),
                    msg,
                }
            }
            (StatusCode::UNAUTHORIZED, None) => ApiError::Unauthorized,
            (code, msg) => ApiError::Unknown { code, msg },
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    // raw response
    #[derive(Deserialize, Debug)]
    struct TestResponse {}

    #[test]
    fn raw_response_success() {
        let input = json!({"ok": true});
        let res: RawResponse<TestResponse> = serde_json::from_value(input).unwrap();

        assert!(matches!(res, RawResponse::Success(TestResponse {})));
    }

    #[test]
    fn raw_response_failure() {
        let input = json!({"ok": false, "msg": "an error occured"});
        let res: RawResponse<TestResponse> = serde_json::from_value(input).unwrap();

        assert!(matches!(res, RawResponse::Failure(msg) if msg == "an error occured"));
    }

    // TODO: test EVERYTHING
}
