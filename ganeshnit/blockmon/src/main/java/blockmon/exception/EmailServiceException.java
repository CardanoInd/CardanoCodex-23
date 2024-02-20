package blockmon.exception;


import java.io.Serial;

public class EmailServiceException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;


    public EmailServiceException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.GENERIC_ERROR.getErrMsgKey();
        this.errorCode = ErrorCode.GENERIC_ERROR.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
