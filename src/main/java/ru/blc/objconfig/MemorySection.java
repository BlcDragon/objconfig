package ru.blc.objconfig;

import com.google.common.base.Preconditions;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.Map.Entry;

public class MemorySection implements ConfigurationSection {

    protected final Map<String, Object> data = new LinkedHashMap<>();
    private ConfigurationSection parent;
    private String key;

    protected MemorySection() {
        this.key = "";
    }

    protected MemorySection(ConfigurationSection parent, String name) {
        Preconditions.checkNotNull(parent, "Parent can not be null");
        Preconditions.checkArgument(!name.isEmpty(), "Name can not be empty or null");
        this.key = name;
        this.parent = parent;
    }

    @Override
    public @NotNull String getPath() {
        if (parent == null) {
            return key;
        }
        String path = parent.getPath() + "." + key;
        if (path.charAt(0) == '.') path = path.substring(1);
        return path;
    }

    @Override
    public @NotNull String getName() {
        return key;
    }

    @Override
    public @Nullable ConfigurationSection getParent() {
        return parent;
    }

    @Override
    public @NotNull Set<String> getKeys() {
        Set<String> keys = new HashSet<>();
        for (Entry<String, Object> entry : data.entrySet()) {
            if (entry.getValue() instanceof List) {
                List<?> list = (List<?>) entry.getValue();
                for (Object obj : list) {
                    if (obj instanceof ConfigurationSection) {
                        keys.add(((ConfigurationSection) obj).getName());
                    } else {
                        keys.add(entry.getKey());
                        break;
                    }
                }
            } else {
                keys.add(entry.getKey());
            }
        }
        return keys;
    }

    @Override
    public @NotNull Set<@NotNull String> getKeysDeep() {
        Set<String> keys = new HashSet<>();
        for (Entry<String, Object> entry : data.entrySet()) {
            if (entry.getValue() instanceof List) {
                List<?> list = (List<?>) entry.getValue();
                for (Object obj : list) {
                    if (obj instanceof ConfigurationSection) {
                        keys.addAll(((ConfigurationSection) obj).getKeysDeep());
                    } else {
                        keys.add(getPath() + "." + entry.getKey());
                        break;
                    }
                }
            } else if (entry.getValue() instanceof ConfigurationSection) {
                keys.addAll(((ConfigurationSection) entry.getValue()).getKeysDeep());
            } else {
                keys.add(getPath() + "." + entry.getKey());
            }
        }
        return keys;
    }

    @Override
    public @NotNull Map<@NotNull String, @NotNull Object> getValues() {
        return new LinkedHashMap<>(data);
    }

    @Override
    public boolean hasValue(@NotNull String path) {
        return get(path) != null;
    }

    protected final Object getFromData(String path) {
        if (path.matches(".+#\\d+")) {
            Object obj = data.get(path.split("#")[0]);
            if (obj == null) return null;
            int nom = Integer.parseInt(path.split("#")[1]);
            if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if (nom >= list.size()) return null;
                return list.get(nom);
            } else {
                return null;
            }
        } else {
            return data.get(path);
        }
    }

    @Override
    public @Nullable Object get(@NotNull String path) {
        if (path.isEmpty()) return this;
        if (path.contains(".")) {
            String nextObjectPath = path.split("\\.")[0];
            Object nextLvlObj = getFromData(nextObjectPath);
            path = path.substring(path.indexOf('.') + 1);
            if (nextLvlObj == null) return null;
            if (nextLvlObj instanceof ConfigurationSection) {
                return ((ConfigurationSection) nextLvlObj).get(path);
            } else return null;
        } else return getFromData(path);
    }

    @Override
    public @NotNull Object getRequired(@NotNull String path) {
        return getRequired0(path, Object.class);
    }

    protected <T> @NotNull T getRequired0(@NotNull String path, @NotNull Class<T> clazz) {
        Object result = get(path);
        if (!clazz.isInstance(result))
            throw new NullPointerException("Required " + clazz.getSimpleName() + " at " + path);
        //noinspection unchecked
        return (T) result;
    }

    @Override
    public Object get(@NotNull String path, Object defaults) {
        Object result = get(path);
        return result == null ? defaults : result;
    }

    protected final void setToData(String path, Object param) {
        if (path.matches(".+#\\d+")) {
            String listpath = path.split("#")[0];
            Object obj = data.get(listpath);
            if (!(obj instanceof List)) {
                obj = new ArrayList<>();
                data.put(listpath, obj);
            }
            int nom = Integer.parseInt(path.split("#")[1]);
            @SuppressWarnings("unchecked")
            List<Object> list = (List<Object>) obj;
            if (list.size() <= nom) {
                list.add(param);
                if (param instanceof MemorySection) {
                    MemorySection ms = (MemorySection) param;
                    ms.key = listpath + "#" + (list.size() - 1);
                }
            } else {
                list.set(nom, param);
            }
        } else {
            data.put(path, param);
        }
    }

    protected final void removeFromData(String path) {
        if (path.matches(".+#\\d+")) {
            String listpath = path.split("#")[0];
            Object obj = data.get(listpath);
            if (obj == null) return;
            int nom = Integer.parseInt(path.split("#")[1]);
            if (obj instanceof List) {
                List<?> list = (List<?>) obj;
                if (nom >= list.size()) return;
                list.remove(nom);
                for (int i = nom; i < list.size(); i++) {
                    Object lobj = list.get(i);
                    if (lobj instanceof MemorySection) {
                        MemorySection ms = (MemorySection) lobj;
                        ms.key = listpath + "#" + i;
                    }
                }
            }
        } else {
            data.remove(path);
        }
    }

    @Override
    public void set(@NotNull String path, @Nullable Object param) {
        Preconditions.checkArgument(!path.isEmpty(), "Cannot set to an empty path");
        if (path.contains(".")) {
            String sectionPath = path.substring(0, path.lastIndexOf('.'));
            String valPath = path.substring(path.lastIndexOf('.') + 1);
            ConfigurationSection section = getConfigurationSection(sectionPath);
            if (section == null) section = createSection(sectionPath);
            section.set(valPath, param);
        } else {
            if (param == null) removeFromData(path);
            else setToData(path, param);
        }
    }

    @Override
    public @NotNull ConfigurationSection createSection(@NotNull String path) {
        Preconditions.checkArgument(!path.isEmpty(), "Cannot create section at empty path");
        ConfigurationSection result;
        if (path.contains(".")) {
            String sectionPath = path.substring(0, path.indexOf('.'));
            path = path.substring(path.indexOf('.') + 1);
            ConfigurationSection section = getConfigurationSection(sectionPath);
            if (section == null) {
                section = new MemorySection(this, sectionPath);
                setToData(sectionPath, section);
            }
            result = section.createSection(path);
        } else {
            result = new MemorySection(this, path);
            setToData(path, result);
        }
        return result;
    }

    @Override
    public @NotNull ConfigurationSection getOrCreateSection(@NotNull String path) {
        ConfigurationSection result = getConfigurationSection(path);
        if (result == null) result = createSection(path);
        return result;
    }

    @Override
    public @NotNull ConfigurationSection createSectionAtList(@NotNull String path) {
        return createSection(path + "#" + getConfigurationSectionList(path).size());
    }

    @Override
    public @NotNull ConfigurationSection createSection(@NotNull String path, @NotNull Map<@NotNull String, @NotNull Object> map) {
        ConfigurationSection result = createSection(path);
        for (Entry<String, Object> entry : map.entrySet()) {
            result.set(entry.getKey(), entry.getValue());
        }
        return result;
    }

    @Override
    public @NotNull ConfigurationSection createSectionAtList(@NotNull String path, @NotNull Map<@NotNull String, @NotNull Object> map) {
        ConfigurationSection result = createSectionAtList(path);
        for (Entry<String, Object> entry : map.entrySet()) {
            result.set(entry.getKey(), entry.getValue());
        }
        return result;
    }

    @Override
    public boolean isConfigurationSection(@NotNull String path) {
        return this.get(path) instanceof ConfigurationSection;
    }

    @Override
    public @Nullable ConfigurationSection getConfigurationSection(@NotNull String path) {
        Object res = this.get(path);
        return (ConfigurationSection) (res instanceof ConfigurationSection ? res : null);
    }

    @Override
    public @NotNull ConfigurationSection getConfigurationSectionRequired(@NotNull String path) {
        return getRequired0(path, ConfigurationSection.class);
    }

    @Override
    public boolean isBoolean(@NotNull String path) {
        Object res = get(path);
        return res instanceof Boolean;
    }

    @Override
    public boolean getBoolean(@NotNull String path) {
        return getBoolean(path, false);
    }

    @Override
    public boolean getBooleanRequired(@NotNull String path) {
        return getRequired0(path, Boolean.class);
    }

    @Override
    public boolean getBoolean(@NotNull String path, boolean defaults) {
        Object res = get(path, defaults);
        return res instanceof Boolean ? (Boolean) res : Boolean.valueOf(defaults);
    }

    @Override
    public boolean isInt(@NotNull String path) {
        Object res = get(path);
        return res instanceof Integer;
    }

    @Override
    public int getInt(@NotNull String path) {
        return getInt(path, 0);
    }

    @Override
    public int getIntRequired(@NotNull String path) {
        return getRequired0(path, Number.class).intValue();
    }

    @Override
    public int getInt(@NotNull String path, int defaults) {
        Object res = get(path, defaults);
        return res instanceof Number ? ((Number) res).intValue() : defaults;
    }

    @Override
    public boolean isLong(@NotNull String path) {
        Object res = get(path);
        return res instanceof Long;
    }

    @Override
    public long getLong(@NotNull String path) {
        return getLong(path, 0);
    }

    @Override
    public long getLongRequired(@NotNull String path) {
        return getRequired0(path, Number.class).longValue();
    }

    @Override
    public long getLong(@NotNull String path, long defaults) {
        Object res = get(path, defaults);
        return res instanceof Number ? ((Number) res).longValue() : defaults;
    }

    @Override
    public boolean isDouble(@NotNull String path) {
        Object res = get(path);
        return res instanceof Double;
    }

    @Override
    public double getDouble(@NotNull String path) {
        return getDouble(path, 0.0);
    }

    @Override
    public double getDoubleRequired(@NotNull String path) {
        return getRequired0(path, Number.class).doubleValue();
    }

    @Override
    public double getDouble(@NotNull String path, double defaults) {
        Object res = get(path, defaults);
        return res instanceof Number ? ((Number) res).doubleValue() : defaults;
    }

    @Override
    public boolean isString(@NotNull String path) {
        Object res = get(path);
        return res instanceof String;
    }

    @Override
    public String getString(@NotNull String path) {
        return getString(path, null);
    }

    @Override
    public String getStringRequired(@NotNull String path) {
        return getRequired0(path, Object.class).toString();
    }

    @SuppressWarnings("ConstantConditions")
    @Override
    public String getString(@NotNull String path, String defaults) {
        Object res = get(path, defaults);
        return res != null ? res.toString() : defaults;
    }

    @Override
    public boolean isList(@NotNull String path) {
        Object res = get(path);
        return res instanceof List<?>;
    }

    @Override
    public List<?> getList(@NotNull String path) {
        return getList(path, null);
    }

    @Override
    public List<?> getList(@NotNull String path, List<?> defaults) {
        Object res = get(path);
        return res instanceof List<?> ? (List<?>) res : defaults;
    }

    @Override
    public List<ConfigurationSection> getConfigurationSectionList(@NotNull String path) {
        List<ConfigurationSection> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if ((object instanceof ConfigurationSection) && !this.isPrimitiveObject(object)) {
                result.add((ConfigurationSection) object);
            }
        }
        return result;
    }

    @Override
    public List<String> getStringList(@NotNull String path) {
        List<String> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof String || this.isPrimitiveObject(object)) {
                result.add(String.valueOf(object));
            }
        }
        return result;
    }

    @Override
    public List<Integer> getIntegerList(@NotNull String path) {
        List<Integer> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Integer) {
                result.add((Integer) object);
            } else if (object instanceof String) {
                try {
                    result.add(Integer.parseInt((String) object));
                } catch (Exception ignore) {
                }
            } else if (object instanceof Character) {
                result.add((int) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).intValue());
            }
        }
        return result;
    }

    @Override
    public List<Boolean> getBooleanList(@NotNull String path) {
        List<Boolean> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Boolean) {
                result.add((Boolean) object);
            } else if (object instanceof String) {
                if (Boolean.TRUE.toString().equals(object)) {
                    result.add(Boolean.TRUE);
                } else if (Boolean.FALSE.toString().equals(object)) {
                    result.add(Boolean.FALSE);
                }
            }
        }
        return result;
    }

    @Override
    public List<Double> getDoubleList(@NotNull String path) {
        List<Double> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Double) {
                result.add((Double) object);
            } else if (object instanceof String) {
                try {
                    result.add(Double.parseDouble((String) object));
                } catch (Exception ignore) {
                }
            } else if (object instanceof Character) {
                result.add((double) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).doubleValue());
            }
        }
        return result;
    }

    @Override
    public List<Float> getFloatList(@NotNull String path) {
        List<Float> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Float) {
                result.add((Float) object);
            } else if (object instanceof String) {
                try {
                    result.add(Float.parseFloat((String) object));
                } catch (Exception ignore) {
                }
            } else if (object instanceof Character) {
                result.add((float) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).floatValue());
            }
        }
        return result;
    }

    @Override
    public List<Long> getLongList(@NotNull String path) {
        List<Long> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Long) {
                result.add((Long) object);
            } else if (object instanceof String) {
                try {
                    result.add(Long.parseLong((String) object));
                } catch (Exception ignore) {
                }
            } else if (object instanceof Character) {
                result.add((long) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).longValue());
            }
        }
        return result;
    }

    @Override
    public List<Short> getShortList(@NotNull String path) {
        List<Short> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Short) {
                result.add((Short) object);
            } else if (object instanceof String) {
                try {
                    result.add(Short.parseShort((String) object));
                } catch (Exception ignore) {
                }
            } else if (object instanceof Character) {
                result.add((short) ((Character) object).charValue());
            } else if (object instanceof Number) {
                result.add(((Number) object).shortValue());
            }
        }
        return result;
    }

    @Override
    public List<Byte> getByteList(@NotNull String path) {
        List<Byte> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Byte) {
                result.add((Byte) object);
            } else if (object instanceof String) {
                try {
                    result.add(Byte.parseByte((String) object));
                } catch (Exception ignore) {
                }
            } else if (object instanceof Character) {
                result.add((byte) ((Character) object).charValue());
            } else if (object instanceof Number) {
                result.add(((Number) object).byteValue());
            }
        }
        return result;
    }

    @Override
    public List<Character> getCharacterList(@NotNull String path) {
        List<Character> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Character) {
                result.add((Character) object);
            } else if (object instanceof String) {
                String str = (String) object;
                if (str.length() == 1) {
                    result.add(str.charAt(0));
                }
            } else if (object instanceof Number) {
                result.add((char) ((Number) object).intValue());
            }
        }
        return result;
    }

    @Override
    @Deprecated
    public List<Boolean> getListBoolean(@NotNull String path) {
        List<Boolean> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Boolean) {
                result.add((Boolean) object);
            } else if (object instanceof String) {
                if (Boolean.TRUE.toString().equals(object)) {
                    result.add(Boolean.TRUE);
                } else if (Boolean.FALSE.toString().equals(object)) {
                    result.add(Boolean.FALSE);
                }
            }
        }
        return result;
    }

    @Override
    @Deprecated
    public @NotNull List<Map<?, ?>> getMapList(@NotNull String path) {
        List<Map<?, ?>> result = new ArrayList<>();
        List<?> in = getList(path);
        if (in == null) return result;
        for (Object object : in) {
            if (object instanceof Map) {
                result.add((Map<?, ?>) object);
            }
        }
        return result;
    }

    protected boolean isPrimitiveObject(Object input) {
        return input instanceof Boolean || input instanceof Character || input instanceof Integer
                || input instanceof Byte || input instanceof Short || input instanceof Double || input instanceof Long
                || input instanceof Float;
    }
}
