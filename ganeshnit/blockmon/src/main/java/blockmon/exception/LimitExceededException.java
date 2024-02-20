package blockmon.exception;


import java.io.Serial;

public class LimitExceededException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public LimitExceededException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.LIMIT_EXCEEDED.getErrMsgKey();
        this.errorCode = ErrorCode.LIMIT_EXCEEDED.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
