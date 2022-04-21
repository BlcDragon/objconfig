package ru.blc.yml;

import org.junit.Assert;
import org.junit.Test;
import ru.blc.objconfig.yml.YamlConfiguration;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Vector;

public class YamlRepresentTest {

    @Test
    public void testCollectionsRepresent() {
        List<?> source = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 0);
        String normal = yamlOf(source);
        String set = yamlOf(new LinkedHashSet<>(source));
        String vector = yamlOf(new Vector<>(source));
        String primitive = yamlOf(source.toArray());
        Assert.assertEquals("set fail", normal, set);
        Assert.assertEquals("vector fail", normal, vector);
        Assert.assertEquals("primitive fail", normal, primitive);
    }

    private static String yamlOf(Object collection) {
        YamlConfiguration c = new YamlConfiguration();
        c.set("collection", collection);
        return c.saveToString();
    }
}
