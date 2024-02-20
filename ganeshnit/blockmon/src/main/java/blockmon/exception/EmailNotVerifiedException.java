package blockmon.exception;


import java.io.Serial;

public class EmailNotVerifiedException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public EmailNotVerifiedException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.EMAIL_NOT_VERIFIED.getErrMsgKey();
        this.errorCode = ErrorCode.EMAIL_NOT_VERIFIED.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
