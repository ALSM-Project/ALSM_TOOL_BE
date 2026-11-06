package jp.co.alsm.common;

import java.util.function.Supplier;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
public class TransactionHandler {
	private static final String MANAGER_NAME = "jpaTransactionManager";

	@Transactional(transactionManager = MANAGER_NAME, propagation = Propagation.REQUIRED)
	public <T> T transaction(Supplier<T> supplier) {
		return supplier.get();
	}

	@Transactional(transactionManager = MANAGER_NAME, propagation = Propagation.REQUIRES_NEW)
	public <T> T newTransaction(Supplier<T> supplier) {
		return supplier.get();
	}

	@Transactional(transactionManager = MANAGER_NAME, propagation = Propagation.NOT_SUPPORTED)
	public <T> T notTransaction(Supplier<T> supplier) {
		return supplier.get();
	}
}