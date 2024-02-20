package blockmon.exception;


import java.io.Serial;

public class UnauthorisedException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public UnauthorisedException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.UNAUTHORIZED.getErrMsgKey();
        this.errorCode = ErrorCode.UNAUTHORIZED.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
