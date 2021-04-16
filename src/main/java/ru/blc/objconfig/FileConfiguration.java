package ru.blc.objconfig;

import com.google.common.io.Files;
import ru.blc.validate.Validate;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Map.Entry;

public abstract class FileConfiguration extends MemorySection {
	
	public void load(String file) throws IOException, InvalidConfigurationException {
		load(new File(file), StandardCharsets.UTF_8);
	}

	public void load(String file, Charset charset) throws IOException, InvalidConfigurationException {
		Validate.notNull(file, "File cannot be null");
		load(new File(file));
	}

	public void load(File file) throws IOException, InvalidConfigurationException {
		load(file, StandardCharsets.UTF_8);
	}

	public void load(File file, Charset charset) throws IOException, InvalidConfigurationException {
		Validate.notNull(file, "File cannot be null");
		FileInputStream stream = new FileInputStream(file);
		this.load(new InputStreamReader(stream, charset));
	}

	public void load(Reader reader) throws IOException, InvalidConfigurationException {
		BufferedReader input = reader instanceof BufferedReader ? (BufferedReader) reader : new BufferedReader(reader);
		StringBuilder builder = new StringBuilder();

		String line;
		try {
			while ((line = input.readLine()) != null) {
				builder.append(line).append('\n');
			}
		} finally {
			input.close();
		}
		this.loadFromString(builder.toString());
	}
	
	public void save(String file) throws IOException {
		save(file, StandardCharsets.UTF_8);
	}

	public void save(String file, Charset charset) throws IOException {
		Validate.notNull(file, "File cannot be null");
		save(new File(file), charset);
	}
	
	public void save(File file) throws IOException {
		save(file, StandardCharsets.UTF_8);
	}

	public void save(File file, Charset charset) throws IOException {
		Validate.notNull(file, "File cannot be null");
		Files.createParentDirs(file);
		String data = this.saveToString();
		try (OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(file), charset)) {
			writer.write(data);
		}
	}
	
	public <T extends FileConfiguration> T convertToAnotherType(Class<T> classz){
		try {
			T f = classz.newInstance();
			f.data.clear();
			for (Entry<String, Object> e: this.getValues().entrySet()) {
				f.data.put(e.getKey(), e.getValue());
			}
			return f;
		} catch (InstantiationException | IllegalAccessException e) {
			e.printStackTrace();
		}
		return null;		
	}
	
	public static <T extends FileConfiguration> T createFromSection(Class<T> classz, ConfigurationSection section){
		try {
			T f = classz.newInstance();
			f.data.clear();
			for (Entry<String, Object> e: section.getValues().entrySet()) {
				f.data.put(e.getKey(), e.getValue());
			}
			return f;
		} catch (InstantiationException | IllegalAccessException e) {
			e.printStackTrace();
		}
		return null;		
	}

	public abstract void loadFromString(String source) throws InvalidConfigurationException;

	public abstract void loadFromString(String source, Charset charset) throws InvalidConfigurationException;

	public abstract String saveToString();

	public abstract String saveToString(Charset charset);
}
