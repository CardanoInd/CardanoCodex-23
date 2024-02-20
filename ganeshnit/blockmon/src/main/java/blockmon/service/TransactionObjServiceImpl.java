package blockmon.service;


import blockmon.entity.TransactionObjEntity;
import blockmon.repository.TransactionObjectRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class TransactionObjServiceImpl implements TransactionObjService {

    TransactionObjectRepository repository;

    @Override
    public TransactionObjEntity getTransactionObj(String refId) {
        return repository.findByRefId(refId).orElseThrow();
    }

    @Override
    public List<TransactionObjEntity> getTransactions(List<String> refIds) {
        return repository.findAllByRefIdIn(refIds);
    }
}