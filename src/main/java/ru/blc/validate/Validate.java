package ru.blc.validate;

import java.util.Collection;
import java.util.Map;

public class Validate {

	public static void notNull(Object object) {
		if (object == null) {
			throw new IllegalArgumentException("Validating object is null");
		}
	}

	public static void notNull(Object object, String message) {
		if (object == null) {
			throw new IllegalArgumentException(message);
		}
	}

	public static void notNull(Object... objects) {
		notNull((Object) objects, "Objects to validate is null");
		for (Object obj : objects) {
			notNull(obj);
		}
	}

	public static void notNull(String message, Object... objects) {
		notNull((Object) objects, "Objects to validate is null");
		for (Object obj : objects) {
			notNull(message, obj);
		}
	}

	public static void notEmpty(Collection<?> collection) {
		if ((collection == null) || (collection.size() == 0)) {
			throw new IllegalArgumentException("Validating collection is empty");
		}
	}

	public static void notEmpty(Collection<?> collection, String message) {
		if ((collection == null) || (collection.size() == 0)) {
			throw new IllegalArgumentException(message);
		}
	}

	public static void notEmpty(Collection<?>... collections) {
		notNull((Object) collections, "Collections to validate is null");
		for (Collection<?> c : collections) {
			notEmpty(c);
		}
	}

	public static void notEmpty(String message, Collection<?>... collections) {
		notNull((Object) collections, "Collections to validate is null");
		for (Collection<?> c : collections) {
			notEmpty(message, c);
		}
	}

	public static void notEmpty(Map<?, ?> map) {
		if ((map == null) || (map.size() == 0)) {
			throw new IllegalArgumentException("Validating map is empty");
		}
	}

	public static void notEmpty(Map<?, ?> map, String message) {
		if ((map == null) || (map.size() == 0)) {
			throw new IllegalArgumentException(message);
		}
	}

	public static void notEmpty(Map<?, ?>... maps) {
		notNull((Object) maps, "Maps to validate is null");
		for (Map<?, ?> map : maps) {
			notEmpty(map);
		}
	}

	public static void notEmpty(String message, Map<?, ?>... maps) {
		notNull((Object) maps, "Maps to validate is null");
		for (Map<?, ?> map : maps) {
			notEmpty(message, map);
		}
	}

	public static void notEmpty(String string) {
		if ((string == null) || (string.length() == 0)) {
			throw new IllegalArgumentException("Validating string is empty");
		}
	}

	public static void notEmpty(String string, String message) {
		if ((string == null) || (string.length() == 0)) {
			throw new IllegalArgumentException(message);
		}
	}

	public static void isTrue(boolean expression) {
		if (!expression) {
			throw new IllegalArgumentException("Validating expression is wrong");
		}
	}

	public static void isTrue(boolean expression, String message) {
		if (!expression) {
			throw new IllegalArgumentException(message);
		}
	}

	public static void isTrue(String message, boolean... expressions) {
		notNull((Object) expressions, "Expressions to validate is null");
		for (boolean expression : expressions) {
			isTrue(message, expression);
		}
	}

	public static void isTrue(boolean... expressions) {
		notNull((Object) expressions, "Expressions to validate is null");
		for (boolean expression : expressions) {
			isTrue(expression);
		}
	}
}
