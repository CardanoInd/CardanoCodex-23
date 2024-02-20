package blockmon.exception;

import com.fasterxml.jackson.core.JsonParseException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.http.converter.HttpMessageNotWritableException;
import org.springframework.web.HttpMediaTypeNotAcceptableException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.server.MethodNotAllowedException;

import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolationException;
import java.time.Instant;
import java.util.Locale;


@ControllerAdvice
@Slf4j
@AllArgsConstructor
public class RestApiErrorHandler {

    @ExceptionHandler(BlockfrostAdaptorException.class)
    public ResponseEntity<Error> handleBlockfrostAdaptorException(HttpServletRequest request, BlockfrostAdaptorException ex,
                                                                  Locale locale) {
        log.error("Error occurred - ", ex);

        Error error = ErrorUtils
                .createError(ex.getErrMsgKey(), ex.getErrorCode().getErrCode(), ex.getHttpStatus().value())
                .setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.BAD_GATEWAY);
    }

    @ExceptionHandler(HttpMediaTypeNotSupportedException.class)
    public ResponseEntity<Error> handleHttpMediaTypeNotSupportedException(HttpServletRequest request,
                                                                          HttpMediaTypeNotSupportedException ex,
                                                                          Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(ErrorCode.HTTP_MEDIATYPE_NOT_SUPPORTED.getErrMsgKey(),
                        ErrorCode.HTTP_MEDIATYPE_NOT_SUPPORTED.getErrCode(),
                        HttpStatus.UNSUPPORTED_MEDIA_TYPE.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        log.info("HttpMediaTypeNotSupportedException :: request.getMethod(): " + request.getMethod());
        return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(HttpMessageNotWritableException.class)
    public ResponseEntity<Error> handleHttpMessageNotWritableException(HttpServletRequest request,
                                                                       HttpMessageNotWritableException ex,
                                                                       Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(ErrorCode.HTTP_MESSAGE_NOT_WRITABLE.getErrMsgKey(),
                        ErrorCode.HTTP_MESSAGE_NOT_WRITABLE.getErrCode(),
                        HttpStatus.UNSUPPORTED_MEDIA_TYPE.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        log.info("HttpMessageNotWritableException :: request.getMethod(): " + request.getMethod());
        return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(HttpMediaTypeNotAcceptableException.class)
    public ResponseEntity<Error> handleHttpMediaTypeNotAcceptableException(HttpServletRequest request,
                                                                           HttpMediaTypeNotAcceptableException ex,
                                                                           Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(ErrorCode.HTTP_MEDIA_TYPE_NOT_ACCEPTABLE.getErrMsgKey(),
                        ErrorCode.HTTP_MEDIA_TYPE_NOT_ACCEPTABLE.getErrCode(),
                        HttpStatus.UNSUPPORTED_MEDIA_TYPE.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        log.info("HttpMediaTypeNotAcceptableException :: request.getMethod(): " + request.getMethod());
        return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<Error> handleHttpMessageNotReadableException(HttpServletRequest request,
                                                                       HttpMessageNotReadableException ex,
                                                                       Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(ErrorCode.HTTP_MESSAGE_NOT_READABLE.getErrMsgKey(),
                        ErrorCode.HTTP_MESSAGE_NOT_READABLE.getErrCode(),
                        HttpStatus.NOT_ACCEPTABLE.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(JsonParseException.class)
    public ResponseEntity<Error> handleJsonParseException(HttpServletRequest request,
                                                          JsonParseException ex,
                                                          Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(ErrorCode.JSON_PARSE_ERROR.getErrMsgKey(),
                        ErrorCode.JSON_PARSE_ERROR.getErrCode(),
                        HttpStatus.NOT_ACCEPTABLE.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    public ResponseEntity<Error> handleHttpRequestMethodNotSupportedException(
            HttpServletRequest request,
            HttpRequestMethodNotSupportedException ex,
            Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(ErrorCode.HTTP_REQUEST_METHOD_NOT_SUPPORTED.getErrMsgKey(),
                        ErrorCode.HTTP_REQUEST_METHOD_NOT_SUPPORTED.getErrCode(),
                        HttpStatus.NOT_IMPLEMENTED.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.NOT_IMPLEMENTED);
    }

    @ExceptionHandler(MethodNotAllowedException.class)
    public ResponseEntity<Error> handleMethodNotAllowedException(
            HttpServletRequest request,
            MethodNotAllowedException ex,
            Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(String
                                .format("%s. Supported methods: %s", ex.getMessage(), ex.getSupportedMethods()),
                        ErrorCode.HTTP_REQUEST_METHOD_NOT_SUPPORTED.getErrCode(),
                        HttpStatus.METHOD_NOT_ALLOWED.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.METHOD_NOT_ALLOWED);
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public ResponseEntity<Error> handleConstraintViolationException(HttpServletRequest request,
                                                                    ConstraintViolationException ex, Locale locale) {
        log.error("Error occurred - ", ex);

        Error error = ErrorUtils
                .createError(
                        String.format("%s %s", ErrorCode.CONSTRAINT_VIOLATION.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.CONSTRAINT_VIOLATION.getErrCode(),
                        HttpStatus.BAD_REQUEST.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<Error> handleIllegalArgumentException(
            HttpServletRequest request,
            IllegalArgumentException ex,
            Locale locale) {
        Error error = ErrorUtils
                .createError(String
                                .format("%s %s", ErrorCode.ILLEGAL_ARGUMENT_EXCEPTION.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.ILLEGAL_ARGUMENT_EXCEPTION.getErrCode(),
                        HttpStatus.BAD_REQUEST.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<Error> handleMethodArgumentTypeMismatchException(
            HttpServletRequest request,
            IllegalArgumentException ex,
            Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(String
                                .format("%s %s", ErrorCode.METHOD_ARGUMENT_TYPE_MISMATCH.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.METHOD_ARGUMENT_TYPE_MISMATCH.getErrCode(),
                        HttpStatus.BAD_REQUEST.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(ResourceNotFoundException.class)
    public ResponseEntity<Error> handleResourceNotFoundException(HttpServletRequest request,
                                                                 ResourceNotFoundException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String.format("%s %s", ErrorCode.RESOURCE_NOT_FOUND.getErrMsgKey(), ex.getMessage()),
                        ex.getErrorCode(),
                        HttpStatus.NOT_FOUND.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(ResourceNotCreatedException.class)
    public ResponseEntity<Error> handleResourceNotCreatedException(HttpServletRequest request,
                                                                   ResourceNotFoundException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String.format("%s %s", ErrorCode.RESOURCE_NOT_CREATED.getErrMsgKey(), ex.getMessage()),
                        ex.getErrorCode(),
                        HttpStatus.CONFLICT.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.CONFLICT);
    }

    @ExceptionHandler(ItemNotFoundException.class)
    public ResponseEntity<Error> handleItemNotFoundException(HttpServletRequest request,
                                                             ItemNotFoundException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String.format("%s %s", ErrorCode.ITEM_NOT_FOUND.getErrMsgKey(), ex.getMessage()),
                        ex.getErrorCode(),
                        HttpStatus.NOT_FOUND.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(ItemAlreadyExistsException.class)
    public ResponseEntity<Error> handleItemAlreadyExistsException(HttpServletRequest request,
                                                                  ItemAlreadyExistsException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.GENERIC_ALREADY_EXISTS.getErrMsgKey(), ex.getMessage()),
                        ex.getErrorCode(),
                        HttpStatus.CONFLICT.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.CONFLICT);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Object> handleMethodArgumentNotValidException(HttpServletRequest request,
                                                                        MethodArgumentNotValidException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.PAYLOAD_VALIDATION_FAILED.getErrMsgKey(), ex.getMessage()),
                        HttpStatus.BAD_REQUEST.name(),
                        HttpStatus.BAD_REQUEST.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(InvalidDataAccessApiUsageException.class)
    public ResponseEntity<Object> handleInvalidDataAccessApiUsageException(HttpServletRequest request,
                                                                           InvalidDataAccessApiUsageException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.ILLEGAL_OPERATION.getErrMsgKey(), ex.getMessage()),
                        HttpStatus.BAD_REQUEST.name(),
                        HttpStatus.BAD_REQUEST.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(TooManyRequestsException.class)
    public ResponseEntity<Object> handleTooManyRequestsException(HttpServletRequest request,
                                                                 TooManyRequestsException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.TOO_MANY_REQUESTS.getErrMsgKey(), ex.getMessage()),
                        HttpStatus.TOO_MANY_REQUESTS.name(),
                        HttpStatus.TOO_MANY_REQUESTS.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.TOO_MANY_REQUESTS);
    }

    @ExceptionHandler(LimitExceededException.class)
    public ResponseEntity<Object> handleLimitExceededException(HttpServletRequest request,
                                                               LimitExceededException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.LIMIT_EXCEEDED.getErrMsgKey(), ex.getMessage()),
                        HttpStatus.TOO_MANY_REQUESTS.name(),
                        HttpStatus.TOO_MANY_REQUESTS.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.TOO_MANY_REQUESTS);
    }

    @ExceptionHandler(EmailServiceException.class)
    public ResponseEntity<Object> handleEmailServiceException(HttpServletRequest request,
                                                              EmailServiceException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.GENERIC_ERROR.getErrMsgKey(), ex.getMessage()),
                        HttpStatus.INTERNAL_SERVER_ERROR.name(),
                        HttpStatus.INTERNAL_SERVER_ERROR.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }


    @ExceptionHandler(EmailNotVerifiedException.class)
    public ResponseEntity<Object> handleEmailNotVerifiedException(HttpServletRequest request,
                                                                  EmailNotVerifiedException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ex.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.EMAIL_NOT_VERIFIED.getErrCode(),
                        HttpStatus.FORBIDDEN.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.FORBIDDEN);
    }


    @ExceptionHandler(DuplicateRequestException.class)
    public ResponseEntity<Object> handleDuplicateRequestException(HttpServletRequest request,
                                                                  DuplicateRequestException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ex.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.GENERIC_ERROR.getErrCode(),
                        HttpStatus.CONFLICT.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.CONFLICT);
    }


    @ExceptionHandler(RuntimeException.class)
    public ResponseEntity<Object> handleRuntimeException(HttpServletRequest request,
                                                         RuntimeException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.GENERIC_ERROR.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.GENERIC_ERROR.getErrCode(),
                        HttpStatus.INTERNAL_SERVER_ERROR.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(UnauthorisedException.class)
    public ResponseEntity<Object> handleUnauthorisedException(HttpServletRequest request,
                                                              RuntimeException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.UNAUTHORIZED.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.UNAUTHORIZED.getErrCode(),
                        HttpStatus.UNAUTHORIZED.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.UNAUTHORIZED);
    }

    @ExceptionHandler(InvalidPayloadException.class)
    public ResponseEntity<Object> handleInvalidPayloadException(HttpServletRequest request,
                                                                RuntimeException ex, Locale locale) {
        log.error("Error occurred - ", ex);
        Error error = ErrorUtils
                .createError(
                        String
                                .format("%s %s", ErrorCode.UNAUTHORIZED.getErrMsgKey(), ex.getMessage()),
                        ErrorCode.PAYLOAD_VALIDATION_FAILED.getErrCode(),
                        HttpStatus.BAD_REQUEST.value()).setUrl(request.getRequestURL().toString())
                .setReqMethod(request.getMethod())
                .setTimestamp(Instant.now());
        return new ResponseEntity<>(error, HttpStatus.BAD_REQUEST);
    }

}
