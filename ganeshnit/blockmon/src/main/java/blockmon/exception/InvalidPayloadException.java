package blockmon.exception;


import java.io.Serial;

public class InvalidPayloadException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public InvalidPayloadException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.PAYLOAD_VALIDATION_FAILED.getErrMsgKey();
        this.errorCode = ErrorCode.PAYLOAD_VALIDATION_FAILED.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
