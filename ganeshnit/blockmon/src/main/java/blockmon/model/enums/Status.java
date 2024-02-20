package blockmon.model.enums;

import blockmon.constants.Constants;

public enum Status {
    NOT_CHECKED(Constants.NOT_CHECKED),
    PENDING(Constants.TX_PENDING),
    TX_ON_CHAIN(Constants.TX_ON_CHAIN);
    public final String value;

    Status(String value) {
        this.value = value;
    }
}
