package ru.blc.objconfig;

import ru.blc.validate.Validate;

import java.util.*;
import java.util.Map.Entry;

public class MemorySection implements ConfigurationSection {

	protected final Map<String, Object> data = new LinkedHashMap<String, Object>();
	private ConfigurationSection parent;
	private String key;

	protected MemorySection() {
		this.key = "";
	}

	protected MemorySection(ConfigurationSection parent, String name) {
		Validate.notNull(parent, "Parent can not be null");
		Validate.notEmpty(name, "Name can not be empty or null");
		this.key = name;
		this.parent = parent;
	}

	public String getPath() {
		if (parent==null) {
			return key;
		}
		String path = parent.getPath()+ "."+key;
		if (path.charAt(0) == '.') path = path.substring(1);
		return path;
	}

	public String getName() {
		return key;
	}

	public ConfigurationSection getParent() {
		return parent;
	}

	public Set<String> getKeys() {
		Set<String> keys = new HashSet<>();
		for (Entry<String, Object> entry : data.entrySet()) {
			if (entry.getValue() instanceof List) {
				List<?> list = (List<?>) entry.getValue();
				for (Object obj :list) {
					if (obj instanceof ConfigurationSection) {
						keys.add(((ConfigurationSection)obj).getName());
					}else {
						keys.add(entry.getKey());
						break;
					}
				}
			}else {
				keys.add(entry.getKey());
			}
		}
		return keys;
	}

	@Override
	public Set<String> getKeysDeep() {
		Set<String> keys = new HashSet<>();
		for (Entry<String, Object> entry : data.entrySet()) {
			if (entry.getValue() instanceof List) {
				List<?> list = (List<?>) entry.getValue();
				for (Object obj :list) {
					if (obj instanceof ConfigurationSection) {
						keys.addAll(((ConfigurationSection)obj).getKeysDeep());
					}else {
						keys.add(getPath()+"."+entry.getKey());
						break;
					}
				}
			}else if (entry.getValue() instanceof ConfigurationSection){
				keys.addAll(((ConfigurationSection)entry.getValue()).getKeysDeep());
			}else {
				keys.add(getPath()+"."+entry.getKey());
			}
		}
		return keys;
	}

	public Map<String, Object> getValues() {
		return new LinkedHashMap<String, Object>(data);
	}

	public boolean hasValue(String path) {
		return get(path)!=null;
	}
	
	protected final Object getFromData(String path) {
		if (path.matches(".+#\\d+")) {
			Object obj = data.get(path.split("#")[0]);
			if (obj==null) return null;
			int nom = Integer.parseInt(path.split("#")[1]);
			if (obj instanceof List) {
				List<?> list = (List<?>) obj;
				if (nom>=list.size()) return null;
				return list.get(nom);
			}else {
				return null;
			}
		}else {
			return data.get(path);
		}
	}
	
	public Object get(String path) {
		if (path == null) return null;
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

	public Object get(String path, Object defaults) {
		Object result = get(path);
		return result == null ? defaults : result;
	}
	
	protected final void setToData(String path, Object param) {
		if (path.matches(".+#\\d+")) {
			String listpath = path.split("#")[0];
			Object obj = data.get(listpath);
			if ((obj == null)||!(obj instanceof List)) {
				obj = new ArrayList<>();
				data.put(listpath, obj);
			}
			int nom = Integer.parseInt(path.split("#")[1]);
			if (obj instanceof List) {
				@SuppressWarnings("unchecked")
				List<Object> list = (List<Object>) obj;
				if (list.size()<=nom) {
					list.add(param);
					if (param instanceof MemorySection) {
						MemorySection ms = (MemorySection) param;
						ms.key = listpath+"#"+(list.size()-1);
					}
				}else {
					list.set(nom, param);
				}
			}
		}else {
			data.put(path, param);
		}
	}
	
	protected final void removeFromData(String path) {
		if (path.matches(".+#\\d+")) {
			String listpath = path.split("#")[0];
			Object obj = data.get(listpath);
			if (obj==null) return;
			int nom = Integer.parseInt(path.split("#")[1]);
			if (obj instanceof List) {
				List<?> list  = (List<?>) obj;
				if (nom>=list.size()) return;
				list.remove(nom);
				for (int i = nom; i<list.size(); i++) {
					Object lobj = list.get(i);
					if (lobj instanceof MemorySection) {
						MemorySection ms = (MemorySection) lobj;
						ms.key = listpath+"#"+i;
					}
				}
			}
		}else {
			data.remove(path);
		}
	}

	public void set(String path, Object param) {
		Validate.notEmpty(path, "Cannot set to an empty path");
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

	public ConfigurationSection createSection(String path) {
		Validate.notEmpty(path, "Cannot create section at empty path");
		ConfigurationSection result = this.getConfigurationSection(path);
		if (result != null) return result;
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
	
	
	public ConfigurationSection createSectionAtList(String path) {
		return createSection(path+"#"+getConfigurationSectionList(path).size());
	}

	public ConfigurationSection createSection(String path, Map<String, Object> map) {
		ConfigurationSection result = createSection(path);
		for (Entry<String, Object> entry : map.entrySet()) {
			result.set(entry.getKey(), entry.getValue());
		}
		return result;
	}
	
	public ConfigurationSection createSectionAtList(String path, Map<String, Object> map) {
		ConfigurationSection result = createSectionAtList(path);
		for (Entry<String, Object> entry : map.entrySet()) {
			result.set(entry.getKey(), entry.getValue());
		}
		return result;
	}


	public boolean isConfiguratinSection(String path) {
		return this.get(path) instanceof ConfigurationSection;
	}

	public ConfigurationSection getConfigurationSection(String path) {
		Object res = this.get(path);
		return (ConfigurationSection) (res instanceof ConfigurationSection ? res : null);
	}

	public boolean isBoolean(String path) {
		Object res = get(path);
		return res instanceof Boolean;
	}

	public boolean getBoolean(String path) {
		return getBoolean(path, false);
	}

	public boolean getBoolean(String path, boolean defaults) {
		Object res = get(path, new Boolean(defaults));
		return res instanceof Boolean ? (Boolean) res : new Boolean(defaults);
	}

	public boolean isInt(String path) {
		Object res = get(path);
		return res instanceof Integer;
	}

	public int getInt(String path) {
		return getInt(path, 0);
	}

	public int getInt(String path, int defaults) {
		Object res = get(path, new Integer(defaults));
		return res instanceof Number ? ((Number) res).intValue() : new Integer(defaults);
	}

	public boolean isLong(String path) {
		Object res = get(path);
		return res instanceof Long;
	}

	public long getLong(String path) {
		return getLong(path, 0);
	}

	public long getLong(String path, long defaults) {
		Object res = get(path, new Long(defaults));
		return res instanceof Number ? ((Number) res).longValue() : new Long(defaults);
	}

	public boolean isDouble(String path) {
		Object res = get(path);
		return res instanceof Double;
	}

	public double getDouble(String path) {
		return getDouble(path, 0.0);
	}

	public double getDouble(String path, double defaults) {
		Object res = get(path, new Double(defaults));
		return res instanceof Number ? ((Number) res).doubleValue() : new Double(defaults);
	}

	public boolean isString(String path) {
		Object res = get(path);
		return res instanceof String;
	}

	public String getString(String path) {
		return getString(path, null);
	}

	public String getString(String path, String defaults) {
		Object res = get(path, defaults);
		return res !=null? res.toString() : defaults;
	}

	public boolean isList(String path) {
		Object res = get(path);
		return res instanceof List<?>;
	}

	public List<?> getList(String path) {
		return getList(path, null);
	}

	public List<?> getList(String path, List<?> defaults) {
		Object res = get(path);
		return res instanceof List<?> ? (List<?>) res : defaults;
	}

	public List<ConfigurationSection> getConfigurationSectionList(String path) {
		List<ConfigurationSection> result = new ArrayList<ConfigurationSection>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if ((object instanceof ConfigurationSection) && !this.isPrimitiveObject(object)) {
				result.add((ConfigurationSection) object);
			}
		}
		return result;
	}
	
	public List<String> getStringList(String path) {
		List<String> result = new ArrayList<String>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof String || this.isPrimitiveObject(object)) {
				result.add(String.valueOf(object));
			}
		}
		return result;
	}

	public List<Integer> getIntegerList(String path) {
		List<Integer> result = new ArrayList<Integer>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Integer) {
				result.add((Integer) object);
			} else if (object instanceof String) {
				try {
					result.add(Integer.valueOf((String) object));
				} catch (Exception arg5) {
					;
				}
			} else if (object instanceof Character) {
				result.add(Integer.valueOf(((Character) object).charValue()));
			} else if (object instanceof Number) {
				result.add(Integer.valueOf(((Number) object).intValue()));
			}
		}
		return result;
	}

	public List<Boolean> getBooleanList(String path) {
		List<Boolean> result = new ArrayList<Boolean>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Boolean) {
				result.add((Boolean) object);
			} else if (object instanceof String) {
				if (Boolean.TRUE.toString().equals(object)) {
					result.add(Boolean.valueOf(true));
				} else if (Boolean.FALSE.toString().equals(object)) {
					result.add(Boolean.valueOf(false));
				}
			}
		}
		return result;
	}

	public List<Double> getDoubleList(String path) {
		List<Double> result = new ArrayList<Double>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Double) {
				result.add((Double) object);
			} else if (object instanceof String) {
				try {
					result.add(Double.valueOf((String) object));
				} catch (Exception arg5) {
					;
				}
			} else if (object instanceof Character) {
				result.add(Double.valueOf((double) ((Character) object).charValue()));
			} else if (object instanceof Number) {
				result.add(Double.valueOf(((Number) object).doubleValue()));
			}
		}
		return result;
	}

	public List<Float> getFloatList(String path) {
		List<Float> result = new ArrayList<Float>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Float) {
				result.add((Float) object);
			} else if (object instanceof String) {
				try {
					result.add(Float.valueOf((String) object));
				} catch (Exception arg5) {
					;
				}
			} else if (object instanceof Character) {
				result.add(Float.valueOf((float) ((Character) object).charValue()));
			} else if (object instanceof Number) {
				result.add(Float.valueOf(((Number) object).floatValue()));
			}
		}
		return result;
	}

	public List<Long> getLongList(String path) {
		List<Long> result = new ArrayList<Long>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Long) {
				result.add((Long) object);
			} else if (object instanceof String) {
				try {
					result.add(Long.valueOf((String) object));
				} catch (Exception arg5) {
					;
				}
			} else if (object instanceof Character) {
				result.add(Long.valueOf((long) ((Character) object).charValue()));
			} else if (object instanceof Number) {
				result.add(Long.valueOf(((Number) object).longValue()));
			}
		}
		return result;
	}

	public List<Short> getShortList(String path) {
		List<Short> result = new ArrayList<Short>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Short) {
				result.add((Short) object);
			} else if (object instanceof String) {
				try {
					result.add(Short.valueOf((String) object));
				} catch (Exception arg5) {
					;
				}
			} else if (object instanceof Character) {
				result.add(Short.valueOf((short) ((Character) object).charValue()));
			} else if (object instanceof Number) {
				result.add(Short.valueOf(((Number) object).shortValue()));
			}
		}
		return result;
	}

	public List<Byte> getByteList(String path) {
		List<Byte> result = new ArrayList<Byte>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Byte) {
				result.add((Byte) object);
			} else if (object instanceof String) {
				try {
					result.add(Byte.valueOf((String) object));
				} catch (Exception arg5) {
					;
				}
			} else if (object instanceof Character) {
				result.add(Byte.valueOf((byte) ((Character) object).charValue()));
			} else if (object instanceof Number) {
				result.add(Byte.valueOf(((Number) object).byteValue()));
			}
		}
		return result;
	}

	public List<Character> getCharacterList(String path) {
		List<Character> result = new ArrayList<Character>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Character) {
				result.add((Character) object);
			} else if (object instanceof String) {
				String str = (String) object;
				if (str.length() == 1) {
					result.add(Character.valueOf(str.charAt(0)));
				}
			} else if (object instanceof Number) {
				result.add(Character.valueOf((char) ((Number) object).intValue()));
			}
		}
		return result;
	}

	public List<Boolean> getListBoolean(String path) {
		List<Boolean> result = new ArrayList<Boolean>();
		List<?> in = getList(path);
		if (in == null) return result;
		for (Object object : in) {
			if (object instanceof Boolean) {
				result.add((Boolean) object);
			} else if (object instanceof String) {
				if (Boolean.TRUE.toString().equals(object)) {
					result.add(Boolean.valueOf(true));
				} else if (Boolean.FALSE.toString().equals(object)) {
					result.add(Boolean.valueOf(false));
				}
			}
		}
		return result;
	}

	public List<Map<?, ?>> getMapList(String path) {
		List<Map<?, ?>> result = new ArrayList<Map<?, ?>>();
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
