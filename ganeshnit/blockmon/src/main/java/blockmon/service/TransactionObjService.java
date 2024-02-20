package blockmon.service;


import blockmon.entity.TransactionObjEntity;

import java.util.List;

public interface TransactionObjService {

    TransactionObjEntity getTransactionObj(String refId);

    List<TransactionObjEntity> getTransactions(List<String> refIds);

}