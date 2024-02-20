package blockmon.exception;


import java.io.Serial;

public class TooManyRequestsException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public TooManyRequestsException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.TOO_MANY_REQUESTS.getErrMsgKey();
        this.errorCode = ErrorCode.TOO_MANY_REQUESTS.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
