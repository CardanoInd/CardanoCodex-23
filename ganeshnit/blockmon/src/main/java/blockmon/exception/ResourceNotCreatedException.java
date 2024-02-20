package blockmon.exception;


import java.io.Serial;

public class ResourceNotCreatedException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final String errorCode;

    public ResourceNotCreatedException() {
        super(ErrorCode.RESOURCE_NOT_CREATED.getErrMsgKey());
        this.errMsgKey = ErrorCode.RESOURCE_NOT_CREATED.getErrMsgKey();
        this.errorCode = ErrorCode.RESOURCE_NOT_CREATED.getErrCode();
    }

    public ResourceNotCreatedException(ErrorCode code) {
        super(code.getErrMsgKey());
        this.errMsgKey = code.getErrMsgKey();
        this.errorCode = code.getErrCode();
    }

    public ResourceNotCreatedException(final String message) {
        super(message);
        this.errMsgKey = ErrorCode.RESOURCE_NOT_CREATED.getErrMsgKey();
        this.errorCode = ErrorCode.RESOURCE_NOT_CREATED.getErrCode();
    }

    public String getErrMsgKey() {
        return errMsgKey;
    }

    public String getErrorCode() {
        return errorCode;
    }
}
