package jp.co.alsm.helper;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;

import jp.co.alsm.common.Constant;
import jp.co.alsm.config.ApplicationProfile;

public class PropertiesHelper {
	private MultipleResourceBundle resourceBundle = null;
	private static Map<String, PropertiesHelper> propertiesMap = new HashMap<>();

	public static PropertiesHelper getInstance(String propertyFile) {
		if (propertiesMap.containsKey(propertyFile)) {
			return propertiesMap.get(propertyFile);
		} else {
			PropertiesHelper properties = new PropertiesHelper(propertyFile);
			propertiesMap.put(propertyFile, properties);
			return properties;
		}
	}

	private PropertiesHelper(String propertyFile) {
		try {
			this.resourceBundle = new MultipleResourceBundle(String.join(Constant.HYPHEN, propertyFile,
					ApplicationProfile.getEnvironment().getActiveProfiles()[0]));
			return;
		} catch (Exception e) {
		}
	}

	public String getProperty(String key) {
		if (this.resourceBundle != null) {
			try {
				return this.resourceBundle.getString(key);
			} catch (MissingResourceException mrex) {
				return Constant.BLANK;
			}
		}
		return Constant.BLANK;
	}

	public Enumeration<String> getKeys() {
		return this.resourceBundle.getKeys();
	}
}
