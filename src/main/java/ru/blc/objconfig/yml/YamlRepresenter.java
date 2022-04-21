package ru.blc.objconfig.yml;

import org.yaml.snakeyaml.DumperOptions.FlowStyle;
import org.yaml.snakeyaml.nodes.Tag;
import org.yaml.snakeyaml.representer.Representer;
import ru.blc.objconfig.ConfigurationSection;

import java.util.Collection;
import java.util.Set;

public class YamlRepresenter extends Representer{

	public YamlRepresenter() {
		this.multiRepresenters.put(Set.class, data -> {
			Set<?> collection = (Set<?>) data;
			return representSequence(Tag.SEQ, collection, FlowStyle.BLOCK);
		});
		this.multiRepresenters.put(ConfigurationSection.class, data -> {
			ConfigurationSection section = (ConfigurationSection) data;
			return representMapping(Tag.MAP, section.getValues(), FlowStyle.BLOCK);
		});
		this.multiRepresenters.put(Collection.class, data -> {
			Collection<?> collection = (Collection<?>) data;
			return representSequence(Tag.SEQ, collection, FlowStyle.BLOCK);
		});
	}
}
