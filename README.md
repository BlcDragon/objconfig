# objconfig - YAML\JSON Config Wrapper

<div align="center">
  <a href="https://github.com/blcdragon/objconfig/blob/master/LICENSE.txt">
    <img src="https://img.shields.io/github/license/blcdragon/objconfig">
  </a>

  <a href="https://github.com/blcdragon/objconfig/issues">
    <img src="https://img.shields.io/github/issues/blcdragon/objconfig">
  </a>

  <a href="https://github.com/blcdragon/objconfig/pulls">
    <img src="https://img.shields.io/github/issues-pr/blcdragon/objconfig">
  </a>

  <a href="https://jitpack.io/#BlcDragon/objconfig">
    <img src="https://jitpack.io/v/BlcDragon/objconfig.svg">
  </a>
</div>

## HowTo

Main interface is <code>ConfigurationSection</code> All configs implements it  
There is also two loaders

- <code>JsonConfiguration</code> for json
- <code>YamlConfiguration</code> for yaml

<pre>
public void loadConfig(String validJson){
    JsonConfiguration jsonConfig = JsonConfiguration.loadConfiguration(validJson);
    //valid json is valid yaml
    YamlConfiguration yamlConfig = YamlConfiguration.loadConfiguration(validJson);
}
</pre>
There is detailed documentation for main classes and simple named methods, so it would not be hard

### Migrate to 2.0.0

There is only one really <b>important</b> point

- <code>createConfigurationSection()</code> now not return section if it exists. This section will be <b>ERASED</b> and
  new created section will be returned, so be carefully
  - <code>getOrCreateSection()</code> method will work as old <code>createConfigurationSection()</code> method
