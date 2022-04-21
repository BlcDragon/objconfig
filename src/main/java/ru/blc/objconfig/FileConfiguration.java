package ru.blc.objconfig;

import com.google.common.base.Preconditions;
import com.google.common.io.Files;
import org.jetbrains.annotations.NotNull;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public abstract class FileConfiguration extends MemorySection {

    public void load(String file) throws IOException, InvalidConfigurationException {
        load(new File(file), StandardCharsets.UTF_8);
    }

    public void load(String file, Charset charset) throws IOException, InvalidConfigurationException {
        Preconditions.checkNotNull(file, "File cannot be null");
        load(new File(file));
    }

    public void load(File file) throws IOException, InvalidConfigurationException {
        load(file, StandardCharsets.UTF_8);
    }

    public void load(File file, Charset charset) throws IOException, InvalidConfigurationException {
        Preconditions.checkNotNull(file, "File cannot be null");
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

    /**
     * Save config to file
     *
     * @param file file path
     * @throws IOException If an I/O error occurs
     */
    public void save(String file) throws IOException {
        save(file, StandardCharsets.UTF_8);
    }

    /**
     * Save config to file with specified charset
     *
     * @param file    file path
     * @param charset charset
     * @throws IOException If an I/O error occurs
     */
    public void save(String file, Charset charset) throws IOException {
        Preconditions.checkNotNull(file, "File cannot be null");
        save(new File(file), charset);
    }

    /**
     * Save config to file
     *
     * @param file file to write
     * @throws IOException If an I/O error occurs
     */
    public void save(File file) throws IOException {
        save(file, StandardCharsets.UTF_8);
    }

    /**
     * Save config to file with specified charset
     *
     * @param file    file to write
     * @param charset charset
     * @throws IOException If an I/O error occurs
     */
    public void save(File file, Charset charset) throws IOException {
        Preconditions.checkNotNull(file, "File cannot be null");
        Files.createParentDirs(file);
        String data = this.saveToString();
        try (OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(file), charset)) {
            writer.write(data);
        }
    }

    /**
     * Converts this file configuration into another type
     *
     * @param clazz class of new type
     * @param <T>   new type
     * @return new configuration of specified type with values from this configuration or null if error occurs
     */
    public <T extends FileConfiguration> T convertToAnotherType(Class<T> clazz) {
        try {
            T f = clazz.newInstance();
            f.data.clear();
            f.data.putAll(this.getValues());
            return f;
        } catch (InstantiationException | IllegalAccessException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Creates configuration of specified type from ConfigurationSection
     *
     * @param clazz   class of configuration
     * @param section section from which create configuration
     * @param <T>     type of configuration
     * @return created configuration or null if error occurs
     */
    public static <T extends FileConfiguration> T createFromSection(@NotNull Class<T> clazz, @NotNull ConfigurationSection section) {
        try {
            T f = clazz.newInstance();
            f.data.clear();
            f.data.putAll(section.getValues());
            return f;
        } catch (InstantiationException | IllegalAccessException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Load configuration from string
     *
     * @param source source string
     * @throws InvalidConfigurationException if configuration source isn't correct
     */
    public abstract void loadFromString(@NotNull String source) throws InvalidConfigurationException;

    /**
     * Load configuration from string with specified charset
     *
     * @param source  source string
     * @param charset charset
     * @throws InvalidConfigurationException if configuration source isn't correct
     */
    public abstract void loadFromString(@NotNull String source, @NotNull Charset charset) throws InvalidConfigurationException;

    /**
     * save configuration to string
     *
     * @return current configuration at string
     */
    public abstract @NotNull String saveToString();

    /**
     * save configuration to string with specified charset
     *
     * @param charset charset
     * @return current configuration at string
     */
    public abstract @NotNull String saveToString(Charset charset);

    @Override
    public String toString() {
        return saveToString();
    }
}
