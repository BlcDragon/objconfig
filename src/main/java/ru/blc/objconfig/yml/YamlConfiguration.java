package ru.blc.objconfig.yml;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.DumperOptions.FlowStyle;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.error.YAMLException;
import org.yaml.snakeyaml.representer.Representer;
import ru.blc.objconfig.ConfigurationSection;
import ru.blc.objconfig.FileConfiguration;
import ru.blc.objconfig.InvalidConfigurationException;
import ru.blc.validate.Validate;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class YamlConfiguration extends FileConfiguration {

	protected final Yaml yaml;
	private final Representer representer = new YamlRepresenter();
	private final DumperOptions dumperOptions = new DumperOptions();

	protected String header = "";

	public YamlConfiguration() {
		this.yaml = new Yaml(representer);
	}

	@Override
	public void loadFromString(String source) throws InvalidConfigurationException {
		loadFromString(source, StandardCharsets.UTF_8);
	}

	@Override
	public void loadFromString(String source, Charset charset) throws InvalidConfigurationException {
		Validate.notNull(source, "Source cannot be null");
		Map<?, ?> in;
		Object res = yaml.load(new ByteArrayInputStream(source.getBytes(charset)));
		try {
			in = (Map<?, ?>) res;
		} catch (YAMLException arg3) {
			throw new InvalidConfigurationException(arg3);
		} catch (ClassCastException arg4) {
			throw new InvalidConfigurationException("Top level is not a Map.");
		}
		this.parseToSections(this, in);
		this.header = readHeader(source);
	}

	protected String readHeader(String source) {
		String[] lines = source.split("\r?\n", -1);
		StringBuilder result = new StringBuilder();
		boolean readingHeader = true;
		
		for (int i = 0; i < lines.length && readingHeader; ++i) {
			String line = lines[i];
			if (line.startsWith("#")) {
				result.append(line).append("\n");
			}else if (line.isEmpty()) {
				result.append("\n");
			}else {
				readingHeader = false;
			}
		}
		return result.toString();
	}

	protected void parseToSections(ConfigurationSection section, Map<?, ?> input) {
		if (input == null) return;
		for (Entry<?, ?> entry : input.entrySet()) {
			if (entry.getKey() == null) continue;
			String key = entry.getKey().toString();
			Object value = entry.getValue();
			if (value instanceof Map) {
				this.parseToSections(section.createSection(key), (Map<?, ?>) value);
			} else if (value instanceof List){
				@SuppressWarnings("unchecked")
				List<Object> value2 = (List<Object>)value;
				for (int i = 0; i < value2.size(); i++) {
					Object lvalue = value2.get(i);
					if (lvalue instanceof Map) {
						ConfigurationSection lsect = section.createSection(key+"#"+i);
						this.parseToSections(lsect, (Map<?, ?>) lvalue);
						value2.set(i, lsect);
					}
				}
				section.set(key, value);
			} else {
				section.set(key, value);
			}
		}
	}

	@Override
	public String saveToString() {
		return saveToString(StandardCharsets.UTF_8);
	}

	@Override
	public String saveToString(Charset charset) {
		this.dumperOptions.setDefaultFlowStyle(FlowStyle.BLOCK);
		this.representer.setDefaultFlowStyle(FlowStyle.BLOCK);
		String dump = this.yaml.dump(this.getValues());
		if (dump.equals("{}\n")) dump = "";
		return header + dump;
	}

	public static YamlConfiguration loadConfiguration(File file) {
		Validate.notNull(file, "File cannot be null");
		YamlConfiguration config = new YamlConfiguration();
		try {
			config.load(file);
		} catch (IOException | InvalidConfigurationException e) {
			e.printStackTrace();
		}
		return config;
	}

	public static YamlConfiguration loadConfiguration(Reader reader) {
		Validate.notNull(reader, "Reader cannot be null");
		YamlConfiguration config = new YamlConfiguration();
		try {
			config.load(reader);
		} catch (IOException | InvalidConfigurationException e) {
			e.printStackTrace();
		}
		return config;
	}
}
