package blockmon.exception;

public enum ErrorCode {
    // Internal Errors: 1 to 0999
    GENERIC_ERROR("CHBE-0001", "The system is unable to complete the request. Contact system support."),
    HTTP_MEDIATYPE_NOT_SUPPORTED("CHBE-0002", "Requested media type is not supported. Please use application/json or application/xml as 'Content-Type' header value"),
    HTTP_MESSAGE_NOT_WRITABLE("CHBE-0003", "Missing 'Accept' header. Please add 'Accept' header."),
    HTTP_MEDIA_TYPE_NOT_ACCEPTABLE("CHBE-0004", "Requested 'Accept' header value is not supported. Please use application/json or application/xml as 'Accept' value"),
    JSON_PARSE_ERROR("CHBE-0005", "Make sure request payload should be a valid JSON object."),
    HTTP_MESSAGE_NOT_READABLE("CHBE-0006", "Make sure request payload should be a valid JSON or XML object according to 'Content-Type'."),
    HTTP_REQUEST_METHOD_NOT_SUPPORTED("CHBE-0007", "Request method not supported."),
    CONSTRAINT_VIOLATION("CHBE-0008", "Validation failed."),
    ILLEGAL_ARGUMENT_EXCEPTION("CHBE-0009", "Invalid data passed."),
    RESOURCE_NOT_FOUND("CHBE-0010", "Requested resource not found."),
    USER_NOT_FOUND("CHBE-0011", "Requested user not found."),
    ITEM_NOT_FOUND("CHBE-0012", "Requested item not found."),
    GENERIC_ALREADY_EXISTS("CHBE-0013", "Already exists."),
    ACCESS_DENIED("CHBE-0014", "Access Denied."),
    UNAUTHORIZED("CHBE-0015", "Unauthorized"),
    PAYLOAD_VALIDATION_FAILED("CHBE-0016", "Payload validation failed."),
    RESOURCE_NOT_CREATED("CHBE-0017", "Requested resource could not be created."),
    METHOD_ARGUMENT_TYPE_MISMATCH("CHBE-00018", "Invalid method argument type passed."),
    ILLEGAL_OPERATION("CHBE-00019", "Action attempted is not allowed."),
    BLOCKFROST_TOO_MANY_REQUESTS("CHBE-00020", "Usage is over limit.."),
    BLOCKFROST_ERROR("CHBE-00021", "Error from blockfrost"),
    TOO_MANY_REQUESTS("CHBE-00022", "Too many requests"),
    LIMIT_EXCEEDED("CHBE-00023", "Limited exceeded"),
    EMAIL_NOT_VERIFIED("CHBE-00024", "Email has not been verified yet."),
    DUPLICATE_REQUEST("CHBE-00025", "Duplicate request");

    private final String errCode;
    private final String errMsgKey;

    ErrorCode(final String errCode, final String errMsgKey) {
        this.errCode = errCode;
        this.errMsgKey = errMsgKey;
    }

    public String getErrCode() {
        return errCode;
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }
}
