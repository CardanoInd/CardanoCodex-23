package blockmon.exception;


import java.io.Serial;

public class ItemAlreadyExistsException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public ItemAlreadyExistsException(ErrorCode code) {
        super(code.getErrMsgKey());
        this.errMsgKey = code.getErrMsgKey();
        this.errorCode = code.getErrCode();
    }

    public ItemAlreadyExistsException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.GENERIC_ALREADY_EXISTS.getErrMsgKey();
        this.errorCode = ErrorCode.GENERIC_ALREADY_EXISTS.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
