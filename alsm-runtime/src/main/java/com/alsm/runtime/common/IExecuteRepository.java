package com.alsm.runtime.common;

import java.sql.SQLException;
import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.Query;

import org.springframework.transaction.support.TransactionTemplate;

public interface IExecuteRepository {
	public Query createNativeQuery(String sql) throws SQLException;

	public List<?> getResultList(Query query) throws SQLException;

	public int executeUpdate(Query query) throws SQLException;

	public EntityManager getEntityManager();

	public TransactionTemplate getTransactionTemplate();

	public void commit();

	public void rollback();

	public void close();
}
