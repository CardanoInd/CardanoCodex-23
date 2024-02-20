/*Status types*/
merge into public.tx_status (id, description,
                             created_stamp, last_updated_stamp)
    values ('NOT_CHECKED', 'Status not checked', '2022-07-21 20:00:00.000000', null),
           ('TX_PENDING', 'Transaction is pending', '2022-07-21 20:00:00.000000', null),
           ('TX_ON_CHAIN', 'Transaction is on chain', '2022-07-21 20:00:00.000000', null),
           ('TX_PROCESS_ERROR', 'Error while processing', '2022-07-21 20:00:00.000000', null);

/*Query targets*/
merge into public.query_target (id, description,
                                created_stamp, last_updated_stamp)
    values ('QT_BLOCKFROST', 'Blockfrost service', '2022-07-30 20:00:00.000000', null),
           ('QT_DANDELION', 'Dandelion service', '2022-07-30 20:00:00.000000', null),
           ('QT_KOIOS', 'Koios API', '2022-09-18 20:00:00.000000', null);

/*Stats targets*/
merge into public.app_stats (stat_id, description, stat_value)
    values ('TX_MON_SUBS', 'Webhooks subscribed', '86');
