'use strict';

var data = {};
function raiseError(lang, category, key, suppress) {
  var missingEntryMsg = get(lang, 'common', 'missing_entry', true) ||
    'Missing entry for language "{0}", category "{1}", key "{2}".';
  throw new Error(String.format(missingEntryMsg, lang, category, key));
}
function get(lang, category, key, suppress) {
  var getter = langList[lang];
  if (!getter && !suppress) {
    raiseError(lang, category, key, suppress);
  }
  var value;
  if (getter) {
    value = getter(category, key);
  }
  if (value) {
    return value;
  }
  else if(!suppress) {
    raiseError(lang, category, key, suppress);
  }
  else {
    return null;
  }

  var langSet = data[lang];
  if (langSet) {
    var cat = langSet[category];
    if (cat) {
      var val = cat[key];
      if (val) {
        return val;
      }
    }
  }
  if (!suppress) {
    var missingEntryMsg = get(lang, 'common', 'missing_entry', true) ||
      'Missing entry for language "{0}", category "{1}", key "{2}".';
    throw new Error(String.format(missingEntryMsg, lang, category, key));
  }
  return null;
}
function set(lang, category, key, value) {
  var langSet = data[lang] || (data[lang] = {});
  var cat = langSet[category] || (langSet[category] = {});
  cat[key] = value;
}

var langList = {};
function register(lang, getter) {
  langList[lang] = getter;
}
module.exports = {
  get: get,
  set: set,
  register: register,
};