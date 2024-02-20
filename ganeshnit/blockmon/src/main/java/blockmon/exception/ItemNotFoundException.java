package blockmon.exception;


import java.io.Serial;

public class ItemNotFoundException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public ItemNotFoundException(ErrorCode code) {
        super(code.getErrMsgKey());
        this.errMsgKey = code.getErrMsgKey();
        this.errorCode = code.getErrCode();
    }

    public ItemNotFoundException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.ITEM_NOT_FOUND.getErrMsgKey();
        this.errorCode = ErrorCode.ITEM_NOT_FOUND.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
