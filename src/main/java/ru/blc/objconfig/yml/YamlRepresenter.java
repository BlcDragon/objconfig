package ru.blc.objconfig.yml;

import org.yaml.snakeyaml.DumperOptions.FlowStyle;
import org.yaml.snakeyaml.nodes.Node;
import org.yaml.snakeyaml.nodes.Tag;
import org.yaml.snakeyaml.representer.Represent;
import org.yaml.snakeyaml.representer.Representer;
import ru.blc.objconfig.ConfigurationSection;

public class YamlRepresenter extends Representer{

	public YamlRepresenter() {
		this.multiRepresenters.put(ConfigurationSection.class, new Represent() {
			public Node representData(Object data) {
				ConfigurationSection section = (ConfigurationSection) data;
				return representMapping(Tag.MAP, section.getValues(), FlowStyle.BLOCK);
//				return representMapping(Tag.MAP, section.getValues(), false);
			}
		});
	}
}
