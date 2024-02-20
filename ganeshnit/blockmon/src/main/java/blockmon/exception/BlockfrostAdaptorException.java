package blockmon.exception;


import lombok.Data;
import org.springframework.http.HttpStatus;

import java.io.Serial;

@Data
public class BlockfrostAdaptorException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;
    private final String errMsgKey;
    private final ErrorCode errorCode;
    private final HttpStatus httpStatus;

}
