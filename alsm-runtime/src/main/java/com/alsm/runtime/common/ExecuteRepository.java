package com.alsm.runtime.common;

import java.sql.SQLException;
import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.PersistenceException;
import jakarta.persistence.Query;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.transaction.interceptor.TransactionInterceptor;
import org.springframework.transaction.support.TransactionTemplate;

import com.alsm.runtime.config.ApplicationContextProvider;

public class ExecuteRepository implements IExecuteRepository {

	@PersistenceContext
	private EntityManager entityManager;

	@Autowired
	@Qualifier("transactionTemplate")
	private TransactionTemplate transactionTemplate;

	public ExecuteRepository() {
		ApplicationContextProvider.autowire(this);
	}

	@Override
	public Query createNativeQuery(String sql) throws SQLException {
		return this.entityManager.createNativeQuery(sql);
	}

	@Override
	public List<?> getResultList(Query query) throws SQLException {
		try {
			return query.getResultList();
		} catch (PersistenceException | IllegalArgumentException e) {
			throw new SQLException(e);
		}
	}

	public int executeUpdate(Query query) throws SQLException {
		try {
			return query.executeUpdate();
		} catch (PersistenceException | IllegalArgumentException e) {
			throw new SQLException(e);
		}
	}

	public EntityManager getEntityManager() {
		return entityManager;
	}

	public TransactionTemplate getTransactionTemplate() {
		return transactionTemplate;
	}

	public void commit() {
	}

	public void close() {
	}

	@Override
	public void rollback() {
		TransactionInterceptor.currentTransactionStatus().setRollbackOnly();
	}
}
