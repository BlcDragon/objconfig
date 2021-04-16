package ru.blc.objconfig;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface ConfigurationSection {

	String getPath();

	String getName();

	ConfigurationSection getParent();

	Set<String> getKeys();

	Set<String> getKeysDeep();

	Map<String, Object> getValues();
	
	boolean hasValue(String path);

	Object get(String path);

	Object get(String path, Object defaults);

	void set(String path, Object param);

	ConfigurationSection createSection(String path);

	/**
	 * Создает секцию в конце списка
	 * @param path путь до листа без номера секции
	 * @return созданную секцию
	 */
	ConfigurationSection createSectionAtList(String path);

	ConfigurationSection createSection(String path, Map<String, Object> map);
	
	/**
	 * Создает секцию в конце списка
	 * @param path - путь до листа без номера секции
	 * @return созданную секцию
	 */
	ConfigurationSection createSectionAtList(String path, Map<String, Object> map);

	boolean isConfiguratinSection(String path);

	ConfigurationSection getConfigurationSection(String path);

	boolean isBoolean(String path);

	boolean getBoolean(String path);

	boolean getBoolean(String path, boolean defaults);

	boolean isInt(String path);

	int getInt(String path);

	int getInt(String path, int defaults);

	boolean isLong(String path);

	long getLong(String path);

	long getLong(String path, long defaults);

	boolean isDouble(String path);

	double getDouble(String path);

	double getDouble(String path, double defaults);

	boolean isString(String path);

	String getString(String path);

	String getString(String path, String defaults);

	boolean isList(String path);

	List<?> getList(String path);

	List<?> getList(String path, List<?> defaults);
	
	List<ConfigurationSection> getConfigurationSectionList(String path);

	List<String> getStringList(String path);

	List<Integer> getIntegerList(String path);

	List<Boolean> getBooleanList(String path);

	List<Double> getDoubleList(String path);

	List<Float> getFloatList(String path);

	List<Long> getLongList(String path);

	List<Short> getShortList(String path);

	List<Byte> getByteList(String path);

	List<Character> getCharacterList(String path);

	List<Boolean> getListBoolean(String path);

	List<Map<?, ?>> getMapList(String path);
}
