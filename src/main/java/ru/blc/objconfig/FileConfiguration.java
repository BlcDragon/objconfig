package ru.blc.objconfig;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import ru.blc.validate.Validate;

import java.io.*;
import java.util.Map.Entry;

public abstract class FileConfiguration extends MemorySection {
	
	public void load(String file) throws IOException, InvalidConfigurationException {
		Validate.notNull(file, "File cannot be null");
		load(new File(file));
	}
	
	public void load(File file) throws IOException, InvalidConfigurationException {
		Validate.notNull(file, "File cannot be null");
		FileInputStream stream = new FileInputStream(file);
		this.load(new InputStreamReader(stream, Charsets.UTF_8));
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
		Validate.notNull(file, "File cannot be null");
		save(new File(file));
	}
	
	public void save(File file) throws IOException {
		Validate.notNull(file, "File cannot be null");
		Files.createParentDirs(file);
		String data = this.saveToString();
		OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(file), Charsets.UTF_8);
		try {
			writer.write(data);
		} finally {
			writer.close();
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

	public abstract String saveToString();
}
