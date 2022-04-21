package ru.blc.objconfig;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Main Interface
 * Represents object (<code>{...}</code>)
 *
 * @author Blc
 */
public interface ConfigurationSection {

    /**
     * Path of section contains this section name, and all parents names divided by dot
     * <p>Example: <code>section1.section2.current</code><br>
     * This path means that your section is here:
     * <pre>
     *     {
     *         "section1": {
     *             "section2": {
     *                 "current": {}
     *             }
     *         }
     *     }
     * </pre>
     * Notice that there is no "root" name and dot for it
     * </p>
     * <p> For sections in list section name contains hashtag and position e.g
     * <code>section1.listSection#2.current</code>
     * This path means that your section is here:
     * <pre>
     *     {
     *         "section1": {
     *             "listSection":
     *             [
     *                  {}, # listSection#0
     *                  {}, # listSection#1
     *                  {   # listSection#2
     *                      "current": {}
     *                  }
     *             ]
     *
     *         }
     *     }
     * </pre>
     *
     * </p>
     *
     * @return path for current configuration section, empty for root section
     */
    @NotNull String getPath();

    /**
     * Name of current section is string names this object in config<br>
     * For root section name is empty string
     * <p>For sections in list section name contains hashtag and position e.g
     * <code>listSection#2</code></p>
     *
     * @return name of current section
     */
    @NotNull String getName();

    /**
     * Section in which the current section is located<br>
     * Root section hasn't parent
     *
     * @return parent section or null
     */
    @Nullable ConfigurationSection getParent();

    /**
     * All available keys in this section<br>
     * Ths set is copy, so you can change it as you want
     *
     * @return keys in this section
     */
    @NotNull Set<@NotNull String> getKeys();

    /**
     * All available keys in this section and in all nested sections (and theirs nested sections etc)<br>
     * Ths set is copy, so you can change it as you want
     *
     * @return keys in this section
     */
    @NotNull Set<@NotNull String> getKeysDeep();

    /**
     * Ths map is copy, bat objects in map is <b>not</b> copies<br>
     * @return Map with all keys and values of current section
     */
    @NotNull Map<@NotNull String, @NotNull Object> getValues();

    /**
     * Check if value exists in specified path
     * @param path path to check
     * @return true if any value in specified path exists, otherwise false
     */
    boolean hasValue(@NotNull String path);

    /**
     * Object in specified path
     * @param path path to object
     * @return object in specified path or null if there isn't any object in this path
     */
    @Nullable Object get(@NotNull String path);

    /**
     * Object in specified path
     * @param path path to object
     * @param defaults default value
     * @return object in specified path or default value if there isn't any object in this path
     */
    @Contract("_, !null -> !null")
    Object get(@NotNull String path, Object defaults);

    /**
     * Set object at specified path.<br>
     * All necessary sections will be created
     * @param path path to set
     * @param param object to set. Null for remove object with current path
     */
    void set(@NotNull String path, @Nullable Object param);

    /**
     * Creates section in specified path<br>
     * If there is section in this path - new section would not be created
     * @param path path for section
     * @return created section
     */
    @NotNull ConfigurationSection createSection(@NotNull String path);
    //at 2.0: If section (or any other object) in specified path already exists - it will be erased

    /**
     * Creates section at the end of list at specified path<br>
     * If there is no list of section at specified path - it will be created (and old value erased)
     *
     * @param path path to list (without hash sigh and number)
     * @return created section
     */
    @NotNull ConfigurationSection createSectionAtList(@NotNull String path);

    /**
     * Creates section in specified path<br>
     * If there is section in this path - new section would not be created
     * @param path path for section
     * @param map values for new section
     * @return created section
     */
    @NotNull ConfigurationSection createSection(@NotNull String path, @NotNull Map<@NotNull String, @NotNull Object> map);
    //at 2.0: If section (or any other object) in specified path already exists - it will be erased

    /**
     * Creates section at the end of list at specified path<br>
     * If there is no list of section at specified path - it will be created (and old value erased)
     *
     * @param path path to list (without hash sigh and number)
     * @param map values for new section
     * @return created section
     */
    @NotNull ConfigurationSection createSectionAtList(@NotNull String path, @NotNull Map<@NotNull String, @NotNull Object> map);

    /**
     * Check if object in specified path is {@link ConfigurationSection}
     * @param path path to object
     * @return true if object in specified path is {@link ConfigurationSection} and exists, othrwise false
     */
    boolean isConfiguratinSection(@NotNull String path); //TODO fix name

    /**
     * Returns configuration section from current path
     * @param path path to section
     * @return section from current path or null if there is no section in current path
     */
    @Nullable ConfigurationSection getConfigurationSection(@NotNull String path);

    /**
     * Check if object in specified path is boolean
     * @param path path to object
     * @return true if object in specified path is boolean and exists, othrwise false
     */
    boolean isBoolean(@NotNull String path);

    /**
     * Returns boolean from specified path. <br>
     * If there is no boolean at specified path - returns false
     * @param path path to boolean
     * @return boolean at path
     */
    boolean getBoolean(@NotNull String path);

    /**
     * Returns boolean from specified path. <br>
     * If there is no boolean at specified path - returns default value
     * @param path path to boolean
     * @param defaults default value
     * @return boolean at path or default value
     */
    boolean getBoolean(@NotNull String path, boolean defaults);

    /**
     * Check if object in specified path is integer
     * @param path path to object
     * @return true if object in specified path is integer and exists, othrwise false
     */
    boolean isInt(@NotNull String path);

    /**
     * Returns integer from specified path. <br>
     * If there is no number at specified path - returns 0
     * @param path path to integer
     * @return integer at path
     * @apiNote If there is any number at specified path it will be cast to int with {@link Number#intValue()}
     */
    int getInt(@NotNull String path);

    /**
     * Returns integer from specified path. <br>
     * If there is no number at specified path - returns defaults
     * @param path path to integer
     * @param defaults default value
     * @return integer at path
     * @apiNote If there is any number at specified path it will be cast to int with {@link Number#intValue()}
     */
    int getInt(@NotNull String path, int defaults);

    /**
     * Check if object in specified path is long
     * @param path path to object
     * @return true if object in specified path is long and exists, othrwise false
     */
    boolean isLong(@NotNull String path);

    /**
     * Returns long from specified path. <br>
     * If there is no number at specified path - returns 0
     * @param path path to long
     * @return long at path
     * @apiNote If there is any number at specified path it will be cast to long with {@link Number#longValue()}
     */
    long getLong(@NotNull String path);

    /**
     * Returns long from specified path. <br>
     * If there is no number at specified path - returns defaults
     * @param path path to long
     * @param defaults default value
     * @return long at path
     * @apiNote If there is any number at specified path it will be cast to long with {@link Number#longValue()}
     */
    long getLong(@NotNull String path, long defaults);

    /**
     * Check if object in specified path is double
     * @param path path to object
     * @return true if object in specified path is double and exists, othrwise false
     */
    boolean isDouble(@NotNull String path);

    /**
     * Returns double from specified path. <br>
     * If there is no number at specified path - returns 0.0
     * @param path path to double
     * @return double at path
     * @apiNote If there is any number at specified path it will be cast to double with {@link Number#doubleValue()}
     */
    double getDouble(@NotNull String path);

    /**
     * Returns double from specified path. <br>
     * If there is no number at specified path - returns defaults
     * @param path path to double
     * @param defaults default value
     * @return double at path
     * @apiNote If there is any number at specified path it will be cast to double with {@link Number#doubleValue()}
     */
    double getDouble(@NotNull String path, double defaults);

    /**
     * Check if object in specified path is string
     * @param path path to object
     * @return true if object in specified path is string and exists, othrwise false
     */
    boolean isString(@NotNull String path);

    /**
     * Returns string from specified path. <br>
     * If there is no string or any object at specified path - returns null
     * @param path path to string
     * @return string at path
     * @apiNote If there is any object at specified path it will be cast to string with {@link Object#toString()}
     */
    String getString(@NotNull String path);

    /**
     * Returns string from specified path. <br>
     * If there is no string or any object at specified path - returns defaults
     * @param path path to string
     * @param defaults default value
     * @return string at path
     * @apiNote If there is any object at specified path it will be cast to string with {@link Object#toString()}
     */
    @Contract("_, !null -> !null")
    String getString(@NotNull String path, @Nullable String defaults);

    /**
     * Check if object in specified path is list
     * @param path path to object
     * @return true if object in specified path is list and exists, othrwise false
     */
    boolean isList(@NotNull String path);

    /**
     * Returns list from specified path. <br>
     * If there is no list at specified path - returns empty list
     * @param path path to list
     * @return list at path
     */
    List<?> getList(@NotNull String path);

    /**
     * Returns list from specified path. <br>
     * If there is no list at specified path - returns defaults
     * @param path path to list
     * @param defaults default value
     * @return list at path
     */
    @Contract("_, !null -> !null")
    List<?> getList(@NotNull String path, List<?> defaults);

    /**
     * Returns list of ConfigurationSections from specified path. <br>
     * If there is no list at specified path, or it does not contain any ConfigurationSection - returns empty list
     * @param path path to list
     * @return list at path
     */
    List<ConfigurationSection> getConfigurationSectionList(@NotNull String path);

    /**
     * Returns list of Strings from specified path. <br>
     * If there is no list at specified path, or it does not contain any String or primitive - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote If there is any primitive at specified path it will be cast to string with {@link String#valueOf(Object)}
     */
    List<String> getStringList(@NotNull String path);

    /**
     * Returns list of Integers from specified path. <br>
     * If there is no list at specified path, or it does not contain any Integers - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Numbers will cast with {@link Number#intValue()}, Strings will be cast with {@link Integer#parseInt(String)}
     */
    List<Integer> getIntegerList(@NotNull String path);

    /**
     * Returns list of booleans from specified path. <br>
     * If there is no list at specified path, or it does not contain any booleans - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Strings will be cast
     */
    List<Boolean> getBooleanList(@NotNull String path);

    /**
     * Returns list of Doubles from specified path. <br>
     * If there is no list at specified path, or it does not contain any Doubles - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Numbers will cast with {@link Number#doubleValue()}, Strings will be cast with {@link Double#parseDouble(String)}
     */
    List<Double> getDoubleList(@NotNull String path);

    /**
     * Returns list of Floats from specified path. <br>
     * If there is no list at specified path, or it does not contain any Floats - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Numbers will cast with {@link Number#floatValue()}, Strings will be cast with {@link Float#parseFloat(String)}
     */
    List<Float> getFloatList(@NotNull String path);

    /**
     * Returns list of Longs from specified path. <br>
     * If there is no list at specified path, or it does not contain any Longs - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Numbers will cast with {@link Number#longValue()}, Strings will be cast with {@link Long#parseLong(String)}
     */
    List<Long> getLongList(@NotNull String path);

    /**
     * Returns list of Shorts from specified path. <br>
     * If there is no list at specified path, or it does not contain any Shorts - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Numbers will cast with {@link Number#shortValue()}, Strings will be cast with {@link Short#parseShort(String)}
     */
    List<Short> getShortList(@NotNull String path);

    /**
     * Returns list of Bytes from specified path. <br>
     * If there is no list at specified path, or it does not contain any Bytes - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Numbers will cast with {@link Number#byteValue()}, Strings will be cast with {@link Byte#parseByte(String)}
     */
    List<Byte> getByteList(@NotNull String path);

    /**
     * Returns list of Bytes from specified path. <br>
     * If there is no list at specified path, or it does not contain any Bytes - returns empty list
     * @param path path to list
     * @return list at path
     * @apiNote Numbers will cast with {@link Number#byteValue()}, Strings will be cast with {@link Byte#parseByte(String)}
     */
    List<Character> getCharacterList(@NotNull String path);

    /**
     * @deprecated for removal <br> use {@link ConfigurationSection#getBooleanList(String)} instead
     * @param path path to list
     * @return list at path
     */
    @Deprecated
    List<Boolean> getListBoolean(@NotNull String path);

    /**
     * @deprecated for removal <br> all maps cast to sections
     * @param path path to map
     * @return map at specified path
     */
    @Deprecated
    @NotNull List<Map<?, ?>> getMapList(@NotNull String path);
}
