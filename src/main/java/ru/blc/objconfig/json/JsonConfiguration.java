package ru.blc.objconfig.json;

import com.google.common.base.Preconditions;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;
import org.jetbrains.annotations.NotNull;
import ru.blc.objconfig.ConfigurationSection;
import ru.blc.objconfig.FileConfiguration;
import ru.blc.objconfig.InvalidConfigurationException;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;

public class JsonConfiguration extends FileConfiguration {

    @Override
    public void loadFromString(@NotNull String source) throws InvalidConfigurationException {
        loadFromString(source, StandardCharsets.UTF_8);
    }

    @Override
    public void loadFromString(@NotNull String source, @NotNull Charset charset) throws InvalidConfigurationException {
        if (source.isEmpty()) return;
        InputStream in = new ByteArrayInputStream(source.getBytes(charset));
        try {
            JsonReader reader = new JsonReader(new InputStreamReader(in, charset));
//            reader.beginObject();
            rootFromJson(reader);
//            if (reader.peek() != JsonToken.END_DOCUMENT) {
//                throw new InvalidConfigurationException("JSON parsed but document not ended");
//            }
            reader.close();
        } catch (IOException e) {
            throw new InvalidConfigurationException(source, e);
        }
    }

    protected void rootFromJson(JsonReader reader) throws IOException, InvalidConfigurationException {
        while (reader.hasNext()) {
            JsonToken jsonToken = reader.peek();
            switch (jsonToken) {
                case BEGIN_ARRAY:
                    reader.beginArray();
                    this.set("root", arrayFromJson(reader, "root", this));
                    break;
                case BEGIN_OBJECT:
                    reader.beginObject();
                    sectionFromJson(reader, this);
                    break;
            }
        }
    }

    protected void sectionFromJson(JsonReader reader, ConfigurationSection section) throws IOException, InvalidConfigurationException {
        String name = "";
        boolean namefounded = false;
        while (reader.hasNext()) {
            JsonToken jsonToken = reader.peek();
            switch (jsonToken) {
                case BEGIN_ARRAY:
                    if (!namefounded) {
                        throw new InvalidConfigurationException("Unexpected array begin at " + reader.getPath());
                    } else {
                        namefounded = false;
                        reader.beginArray();
                        section.set(name, arrayFromJson(reader, name, section));
                    }
                    break;
                case BEGIN_OBJECT:
                    if (!namefounded) {
                        throw new InvalidConfigurationException("Unexpected object begin at " + reader.getPath());
                    } else {
                        namefounded = false;
                        reader.beginObject();
                        sectionFromJson(reader, section.createSection(name));
                    }
                    break;
                case BOOLEAN:
                    if (!namefounded) {
                        throw new InvalidConfigurationException("Unexpected " + jsonToken.toString().toLowerCase() + " at " + reader.getPath());
                    } else {
                        namefounded = false;
                        section.set(name, reader.nextBoolean());
                    }
                    break;
                case NAME:
                    if (namefounded) {
                        throw new InvalidConfigurationException("Unexpected " + jsonToken.toString().toLowerCase() + " at " + reader.getPath());
                    } else {
                        namefounded = true;
                        name = reader.nextName();
                    }
                    break;
                case NUMBER:
                    if (!namefounded) {
                        throw new InvalidConfigurationException("Unexpected " + jsonToken.toString().toLowerCase() + " at " + reader.getPath());
                    } else {
                        namefounded = false;
                        section.set(name, getNumber(reader));
                    }
                    break;
                case STRING:
                    if (!namefounded) {
                        throw new InvalidConfigurationException("Unexpected " + jsonToken.toString().toLowerCase() + " at " + reader.getPath());
                    } else {
                        namefounded = false;
                        section.set(name, reader.nextString());
                    }
                    break;
                case NULL:
                    if (!namefounded) {
                        throw new InvalidConfigurationException("Unexpected " + jsonToken.toString().toLowerCase() + " at " + reader.getPath());
                    } else {
                        namefounded = false;
                        section.set(name, null);
                        reader.nextNull();
                    }
                    break;
                case END_ARRAY:
                case END_DOCUMENT:
                case END_OBJECT:
                    throw new InvalidConfigurationException("Unexpected " + jsonToken.toString().toLowerCase().substring("end_".length()) + " end at " + reader.getPath());
            }
        }
        reader.endObject();
    }

    protected List<?> arrayFromJson(JsonReader reader, String arrayname, ConfigurationSection owner) throws IOException, InvalidConfigurationException {
        List<Object> list = new ArrayList<>();
        while (reader.hasNext()) {
            JsonToken jsonToken = reader.peek();
            switch (jsonToken) {
                case STRING:
                    list.add(reader.nextString());
                    break;
                case NUMBER:
                    list.add(getNumber(reader));
                    break;
                case BOOLEAN:
                    list.add(reader.nextBoolean());
                    break;
                case NULL:
                    reader.nextNull();
                    break;
                case BEGIN_ARRAY:
                    reader.beginArray();
                    list.add(arrayFromJson(reader, arrayname, owner));
                    break;
                case BEGIN_OBJECT:
                    reader.beginObject();
                    ConfigurationSection newsection = owner.createSection(arrayname + "#" + list.size());
                    sectionFromJson(reader, newsection);
                    list.add(newsection);
                    break;
                default:
                    throw new InvalidConfigurationException("Founded " + jsonToken + " at array " + reader.getPath());
            }
        }
        reader.endArray();
        return list;
    }

    protected Number getNumber(JsonReader reader) throws IOException, InvalidConfigurationException {
        String num = reader.nextString();
        if (num.matches("-?\\d+\\.\\d+")) {
            return new Double(num);
        }
        if (num.matches("-?\\d+")) {
            long l = Long.parseLong(num);
            if (l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                return new Integer(num);
            } else {
                return l;
            }

        }
        throw new InvalidConfigurationException("Number \"" + num + "\" can not be parsed");
    }

    @Override
    public @NotNull String saveToString() {
        return saveToString(StandardCharsets.UTF_8);
    }

    @Override
    public @NotNull String saveToString(Charset charset) {
        OutputStream outputStream = new ByteArrayOutputStream();
        try {
            JsonWriter writer = new JsonWriter(new OutputStreamWriter(outputStream, charset));
            writer.beginObject();
            sectionToJson(writer, this);
            writer.endObject();
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return outputStream.toString();
    }

    protected void sectionToJson(JsonWriter writer, ConfigurationSection section) throws IOException {
        for (Entry<String, Object> ent : section.getValues().entrySet()) {
            writer.name(ent.getKey());
            Object o = ent.getValue();
            if (o == null) {
                writer.nullValue();
                continue;
            }
            if (o instanceof Collection<?>) {
                writer.beginArray();
                arrayToJson(writer, (List<?>) o);
                writer.endArray();
                continue;
            }
            if (o.getClass().isArray()) {
                writer.beginArray();
                primitiveArrayToJson(writer, o);
                writer.endArray();
                continue;
            }
            if (o instanceof ConfigurationSection) {
                ConfigurationSection s = (ConfigurationSection) o;
                writer.beginObject();
                sectionToJson(writer, s);
                writer.endObject();
                continue;
            }
            simpleObjectToJson(writer, o);
        }
    }

    protected void arrayToJson(JsonWriter writer, List<?> array) throws IOException {
        for (Object o : array) {
            if (o == null) {
                writer.nullValue();
                continue;
            }
            if (o instanceof ConfigurationSection) {
                writer.beginObject();
                sectionToJson(writer, (ConfigurationSection) o);
                writer.endObject();
                continue;
            }
            if (o instanceof List<?>) {
                writer.beginArray();
                arrayToJson(writer, (List<?>) o);
                writer.endArray();
                continue;
            }
            if (o.getClass().isArray()) {
                writer.beginArray();
                primitiveArrayToJson(writer, o);
                writer.endArray();
                continue;
            }
            simpleObjectToJson(writer, o);
        }
    }

    protected void simpleObjectToJson(JsonWriter writer, Object object) throws IOException {
        if (object instanceof String) {
            writer.value((String) object);
        } else if (object instanceof Boolean) {
            writer.value((Boolean) object);
        } else if (object instanceof Number) {
            writer.value((Number) object);
        }
    }

    protected void primitiveArrayToJson(JsonWriter writer, Object array) throws IOException {
        if (array.getClass().getComponentType() == null) {
            for (Object o : (Object[]) array) {
                writer.value(o.toString());
            }
        } else if (array.getClass().getComponentType() == boolean.class) {
            for (boolean o : (boolean[]) array) {
                writer.value(o);
            }

        } else if (array.getClass().getComponentType() == byte.class) {
            for (byte o : (byte[]) array) {
                writer.value(o);
            }

        } else if (array.getClass().getComponentType() == short.class) {
            for (short o : (short[]) array) {
                writer.value(o);
            }

        } else if (array.getClass().getComponentType() == int.class) {
            for (int o : (int[]) array) {
                writer.value(o);
            }

        } else if (array.getClass().getComponentType() == long.class) {
            for (long o : (long[]) array) {
                writer.value(o);
            }

        } else if (array.getClass().getComponentType() == float.class) {
            for (float o : (float[]) array) {
                writer.value(o);
            }

        } else if (array.getClass().getComponentType() == double.class) {
            for (double o : (double[]) array) {
                writer.value(o);
            }

        } else if (array.getClass().getComponentType() == char.class) {
            for (char o : (char[]) array) {
                writer.value(o);
            }

        }
    }

    /**
     * @return new empty json
     */
    public static @NotNull JsonConfiguration empty() {
        return new JsonConfiguration();
    }

    /**
     * load json from string
     *
     * @param json string with json
     * @return loaded json
     */
    public static @NotNull JsonConfiguration loadConfiguration(@NotNull String json) {
        Preconditions.checkNotNull(json, "Source string cannot be null");
        JsonConfiguration config = new JsonConfiguration();
        try {
            config.loadFromString(json);
        } catch (InvalidConfigurationException e) {
            e.printStackTrace();
        }
        return config;
    }

    /**
     * load json from file
     *
     * @param file file with json
     * @return loaded json
     */
    public static @NotNull JsonConfiguration loadConfiguration(@NotNull File file) {
        Preconditions.checkNotNull(file, "File cannot be null");
        JsonConfiguration config = new JsonConfiguration();
        try {
            config.load(file);
        } catch (IOException | InvalidConfigurationException e) {
            e.printStackTrace();
        }
        return config;
    }

    /**
     * load json from reader
     *
     * @param reader reader with json
     * @return loaded json
     */
    public static @NotNull JsonConfiguration loadConfiguration(@NotNull Reader reader) {
        Preconditions.checkNotNull(reader, "Reader cannot be null");
        JsonConfiguration config = new JsonConfiguration();
        try {
            config.load(reader);
        } catch (IOException | InvalidConfigurationException e) {
            e.printStackTrace();
        }
        return config;
    }

    public static JsonConfiguration createFromSection(@NotNull ConfigurationSection section) {
        return FileConfiguration.createFromSection(JsonConfiguration.class, section);
    }
}
