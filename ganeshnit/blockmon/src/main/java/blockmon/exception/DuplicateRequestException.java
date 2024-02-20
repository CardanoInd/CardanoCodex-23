package blockmon.exception;


import java.io.Serial;

public class DuplicateRequestException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public DuplicateRequestException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.DUPLICATE_REQUEST.getErrMsgKey();
        this.errorCode = ErrorCode.DUPLICATE_REQUEST.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
