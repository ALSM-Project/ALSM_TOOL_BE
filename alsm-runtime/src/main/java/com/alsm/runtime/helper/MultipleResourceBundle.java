package com.alsm.runtime.helper;

import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

public class MultipleResourceBundle extends ResourceBundle {

	private String[] bundleNames;
	private Locale locale;
	private Map<String, String> combined;

	public MultipleResourceBundle(String... bundleNames) {
		this.bundleNames = bundleNames;
		this.loadBundlesOnce();
	}

	public MultipleResourceBundle(Locale locale, String... bundleNames) {
		this.locale = locale;
		this.bundleNames = bundleNames;
		this.loadBundlesOnce();
	}

	@Override
	public Object handleGetObject(String key) {
		if (key == null) {
			throw new NullPointerException();
		}
		return this.combined.get(key);
	}

	@Override
	public Enumeration<String> getKeys() {
		return Collections.enumeration(this.combined.keySet());
	}

	private void loadBundlesOnce() {
		if (this.combined == null) {
			this.combined = new HashMap<String, String>();

			for (String bundleName : this.bundleNames) {
				ResourceBundle bundle = this.locale == null ? ResourceBundle.getBundle(bundleName)
						: ResourceBundle.getBundle(bundleName, this.locale);
				Enumeration<String> keys = bundle.getKeys();
				String key = null;
				while (keys.hasMoreElements()) {
					key = keys.nextElement();
					this.combined.put(key, bundle.getString(key));
				}
			}
		}
	}

}