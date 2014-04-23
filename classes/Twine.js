/**
 * Twine
 *
 * @author  DJ Hayden <dj.hayden@stablekernel.com>
 */

/**
 * Module Dependencies
 */
var _util       = require('util'),
    _mongoose   = require('mongoose'),
    _thisapp    = require('../server.js'),
    _request    = require('request'),
    _logger     = require(process.env.PRISM_HOME + 'logs.js'),
    _           = require('underscore'),
    User        = require('../models/user').User;

/**
 * Twine Constants
 */
var DEFAULT_LIMIT = 30;
var DEFAULT_SORT_BY = 'create_date';
var DEFAULT_SORT = -1;
var DEFAULT_PAGE_BY = 'create_date';
var DEFAULT_PAGE_DIRECTION = -1;
var DEFAULT_PAGE = null;
var DEFAULT_FIELDS = null;
var DEFAULT_CONTAINS = null;
var DEFAULT_RESOLVE = null;
var DEFAULT_FETCH = null;
var DEFAULT_IS_CHILD_MODEL = null;
var DEFAULT_CHILD_MODEL_NAME = null;
var DEFAULT_CHILD_MODEL = null;
var DEFAULT_MODEL_KEYS = [];
var DEFAULT_FILTER = {};
var X_ARGUMENTS_HEADER = 'x-arguments';

/**
 * Expose 'Twine'
 */
module.exports = Twine;

function Twine(model, criteria, Request, options, callback){
  var self = this;
  //setup corresponding model to operate on
  this.Model = _mongoose.model(model);
  //setup model schema from passed model string
  this.Schema = this.Model().schema;
  //setup init find criteria
  this.criteria = criteria;
  //set http request object
  this.Request = Request;
  //set http arguments
  this.args = self.$__digestHeaderArguments();
  //set optional options hash
  this.options = options;
  //set callback block
  this.cb = callback;
  //setup sort
  this.sort = self.$__setupProperty('sort', DEFAULT_SORT);
  //setup sort_by
  this.sort_by = self.$__setupProperty('sort_by', DEFAULT_SORT_BY);
  //setup limit
  this.limit = self.$__setupProperty('limit', DEFAULT_LIMIT);
  //setup fields
  this.fields = self.$__setupProperty('fields', DEFAULT_FIELDS);
  //setup page
  this.page = self.$__setupProperty('page', DEFAULT_PAGE);
  //setup page_by
  this.page_by = self.$__setupProperty('page_by', DEFAULT_PAGE_BY);
  //setup page_direction
  this.page_direction = self.$__setupProperty('page_direction', DEFAULT_PAGE_DIRECTION);
  //setup filters
  this.filters = DEFAULT_FILTER;
  //setup contains
  this.contains = self.$__setupProperty('contains', DEFAULT_CONTAINS);
  //setup resolves
  this.resolves = self.$__setupProperty('resolve', DEFAULT_RESOLVE);
  //setup fetch
  this.fetch = DEFAULT_FETCH;
  //setup has_child_model
  this.has_child_model = self.$__setupProperty('is_child_model', DEFAULT_IS_CHILD_MODEL);
  //setup child model
  this.child_model = self.$__setupProperty('child_model', DEFAULT_CHILD_MODEL);
  //setup child model name
  this.child_model_name = DEFAULT_CHILD_MODEL_NAME;
  if(this.options){
    if(_.has(options, 'child_model')){
      this.child_model_name = options.child_model;
    }
  }
  //setup model keys
  this.model_keys = DEFAULT_MODEL_KEYS;
  //intro the Schema & set available model keys
  for(var key in this.Schema.paths){
    this.model_keys.push(key);
  }

  self.$__resolveFilterProperties();
  self.buildBaseRequest();
}

Twine.prototype.$__digestHeaderArguments = function $__digestHeaderArguments(){
  var res = null;
  if(this.Request && _.has(this.Request.headers, X_ARGUMENTS_HEADER)){
    //base64 decode the x-args header
    var args = new Buffer(this.Request.headers[X_ARGUMENTS_HEADER], 'base64').toString('utf8');
    res = JSON.parse(args);
    _logger.log('info', 'digested header arguments', {"x-arguments": res});
  }
  return res;
};

Twine.prototype.$__setupProperty = function $__setupProperty(option, prop_default){
    if(this.options){
      if(_.has(this.options, option)){
        return this.options[option];
      }else{
        return prop_default;
      }
    }else{
      return prop_default;
    }
};

Twine.prototype.$__isSchemaProperty = function $__isSchemaProperty(property){
  for(var prop in this.Schema.paths){
    if(prop === property) return true;
  }
  return false;
};

Twine.prototype.$__isQuerySet = function $__isQuerySet(){
  return (_.has(this.Request, 'query'));
};

Twine.prototype.$__isBodySet = function $__isBodySet(){
  return (_.has(this.Request, 'body'));
};

Twine.prototype.$__parse = function $__parse(param, default_value){
    var header = null;
    var qs = null;
    if(this.args)
      header = (typeof this.args[param] !== 'undefined') ? this.args[param] : null;
    if(this.Request.query)
      qs = (typeof this.Request.query[param] !== 'undefined') ? this.Request.query[param] : null;
    return (header || qs) ? ((qs) ? qs : header) : default_value;
};

Twine.prototype.$__resolveFilterProperties = function $__resolveFilterProperties(){
  var findFilterProperties = function findFilterProperties(object, model_keys, filters){
    for(var filter in object){
      if(model_keys.indexOf(filter) !== -1){
        filters[filter] = object[filter];
      }
    }
  };
  if(this.$__isQuerySet()) findFilterProperties(this.Request.query, this.model_keys, this.filters);
  if(this.$__isBodySet()) findFilterProperties(this.Request.body, this.model_keys, this.filters);
  if(this.args) findFilterProperties(this.args, this.model_keys, this.filters);
};

Twine.prototype.$__setPage = function $__setPage(){
 if(this.page){
    if(this.page_direction === 1){
      this.criteria[this.page_by] = {$lt : this.page};
    }else if(this.page_direction === 0){
      this.criteria[this.page_by] = {$gt : this.page};
    }else{
      this.criteria[this.page_by] = {$gt : this.page};
    }
  }
};

Twine.prototype.$__setSelect = function $__setSelect(){
  var select = this.Model.selectFields('basic');
  if(this.contains){
    var keys = Object.keys(this.contains);
    for(var i in keys){
      if(select.indexOf(keys[i]) === -1) select.push(keys[i]);
    }
  }
  if(this.resolve){
    var r_keys = Object.keys(this.resolve);
    for(var r in r_keys){
      if(select.indexOf(r_keys[r]) === -1) select.push(r_keys[r]);
    }
  }
  if(this.fields){
    var array = (Array.isArray(this.fields)) ? this.fields : this.fields.split(" ");
    for(var a in array){
      if(select.indexOf(array[a]) === -1) select.push(array[a]);
    }
  }

  this.fetch.select(select.join(" "));
};

Twine.prototype.$__setSort = function $__setSort(){
  if(this.sort){
   var sort = {};
   sort[this.sort_by] = this.sort;
   if(this.sort_by && this.sort_by !== DEFAULT_PAGE_BY) sort[DEFAULT_PAGE_BY] = DEFAULT_PAGE_DIRECTION;
   _logger.log('info', 'setting sort', {sort: sort});
   this.fetch.sort(sort);
  }
};

Twine.prototype.$__setLimit = function $__setLimit(){
  this.fetch.limit(this.limit);
};

Twine.prototype.$__setFilters = function $__setFilters(cb){
  //amend the filters to the existing criteria if exists
  if(this.filters){
    for(var key in this.filters){
      this.criteria[key] = this.filters[key];
    }
  }
};

Twine.prototype.buildBaseRequest = function buildBaseRequest (){
  //set the filters, which ammend the search criteria for the base request
  this.$__setFilters();
  //set the page, page_direction, & page_by
  this.$__setPage();
  //set intial fetch object with base criteria;
  this.fetch = this.Model.find(this.criteria);
  //ammend fetch with  sort, filter, & select fields
  this.$__setSelect();
  this.$__setSort();
  this.$__setLimit();
  //only execute the request if the callback is set. if not
  //the executerequest can take an optional cb -- to invoked seperately
  //TODO: worry about this on second pass.. just call for now
  //if(this.cb) this.executeRequest();
  this.executeRequest();
};

Twine.prototype.executeRequest = function executeRequest (){
  var response = {};
  var self = this;
  self.fetch.exec(function(err, result){
    //if there is an error returned from the fetch its server related
    //so send it back immediately
    if(err) self.cb(err, null);
    //if the result set is empty, invoke callback immediately, else process
    if(_.isEmpty(result)){
      self.cb(err, result);
    }else{
      //set the base result set in the response object
      response.data = result;
      //if contains or resolve properties are set process, else return response
      if(!self.contains && !self.resolve){
        self.cb(err, response);
      }else{
        self.process(response, function(err, process_result){
          self.cb(err, process_result);
        });
      }
    }
  });
};

Twine.prototype.$__formatChildModelBaseResults = function(base, model_name){
  //pluralize the child model name to match the naaming convention of
  //parent model property
  model_name = this.$__formatMongooseModelStringForProperty(model_name);
  for(var index in base.data){
    if(typeof base.data[index][model_name] !== 'undefined')
      base.data[index] = base.data[index][model_name];
  }
  return base;
};

Twine.prototype.process = function process (base, block){
  var container = {};
  var self = this;
  if(self.has_child_model){
    base = self.$__formatChildModelBaseResults(base, self.child_model_name);
  }
  //if contains does exist send to resolve -- if nothing to resolve it will invoke
  //the callback `block` and finish the process, thus retuning the result
  if(!self.contains){
    self.processResolve(base, self.resolve, container, block);
  }else{
    self.processContains(base, self.contains, function(base){
      //bubble up error?
      if(!self.resolve){
        block(false, base);
      }else{
        self.processResolve(base, self.resolve, container, block);
      }
    });
  }
};

Twine.prototype.processContains = function processContains(base, contains, block){
  _logger.log('info', 'contains to be processed', {contains: contains});
  for(var contain in contains){
    //set contain key
    var k = Object.keys(contains[contain])[0];
    _logger.log('info','key', {key: k});
    //set contain value
    var v = contains[contain][k];
    _logger.log('info', 'value', {value: v});
    //loop through each array in the result set & compare to contains key, value
    _logger.log('info', 'contain loop key value', {contain:contain, contains:contains, key:k, value:v});

    if(typeof base.data === 'undefined'){
      base.data = base;
    }

    for(var num in base.data){
      var found = false;
      var has_key = false;
      _logger.log('info', 'index of base.data to be evaluated', {object: base.data[num]});
      if(_.has(base.data[num], contain)){
        has_key = true;
        for(var check in base.data[num][contain]){
          _logger.log('info', 'does base.data index ' + num +
                      ' field ' +contain+ ' field index + '+check+' key + '+k+' == value: '+v);
          _logger.log('info','field value to compare: ' + JSON.stringify(base.data[num][contain][check]));
          if(typeof base.data[num][contain][check] !== 'undefined')
            if(typeof base.data[num][contain][check][k] !== 'undefined')
              if(base.data[num][contain][check][k] === v){
                _logger.log('info', 'contains key value found: ' +base.data[num][contain][check][k]+ ' matching v value: '+ v);
                var key_value_res = {};
                key_value_res[k] = v;
                base.data[num][contain] = [key_value_res];
                found = true;
              }
        }
        if(!found && has_key) base.data[num][contain] = [];
      }else if(Array.isArray(base.data[num])){
        for(var i in base.data[num]){
          if(_.has(base.data[num][i], contain)){
            has_key = true;
            for(var check in base.data[num][i][contain]){
              if(typeof base.data[num][i][contain][check] !== 'undefined' &&
                 typeof base.data[num][i][contain][check][k] !== 'undefined'){
                if(base.data[num][i][contain][check][k] === v){
                  var key_value_res = {};
                  key_value_res[k] = v;
                  base.data[num][i][contain] = key_value_res;
                  found = true;
                }
              }
            }
          }
          if(!found && has_key) base.data[num][i][contain] = [];
        }
      }
    }
  }
  block(base);
};

/**
 * resolve process
 * 1. get the Model.canResolve objects & identift the resolve field, & format
 * 2. get distinct identifiers to resolve from base object property key/value
 * 3. fetchResolveRequest
 * 4. push values into resolve reponse key
 * 5. check to see if there is a contains on the resolve key if so
 */

Twine.prototype.canResolve = function canResolve(field){
  var model_resolve_fields = (this.has_child_model) ?
    this.child_model.canResolve() : this.Model.canResolve();
  for(var i in model_resolve_fields){
    if(Object.keys(model_resolve_fields[i])[0] === field) return model_resolve_fields[i];
  }
};

Twine.prototype.$__formatStringForMongooseModel = function $__formatStringForMongooseModel(string){
  string = string.charAt(0).toUpperCase() + string.toLowerCase().slice(1);
  if(string.charAt(string.length -1) === 's')
    string = string.substring(0, string.length -1);
  return string;
};

Twine.prototype.$__formatMongooseModelStringForProperty = function(string){
  string = string.toLowerCase();
  if(string.charAt(string.length -1) !== 's') return string + 's';
  return string;
};

Twine.prototype.$__isEmbeddedModelObject = function $__isEmbeddedModelObject(model_string, cb){
  var model = this.$__formatStringForMongooseModel(model_string);
  if(typeof _mongoose.model(model) !== 'undefined'){
    return true;
  }
  return false;
};

Twine.prototype.getDistinctValuesForField = function getDistinctValuesForField(object,id,field){
  var distinct_array = [];
  _logger.log('info', 'object, id, field to getch distinct values', {field:field, id:id, object:object});
  _logger.log('info', 'is object an array: ' + (Array.isArray(object)));
  if(Array.isArray(object)){
    for(var index in object){
      object[index] = (typeof object[index].toObject === 'function')? object[index].toObject() : object[index];
      if(typeof object[index][field] == 'undefined' && Array.isArray(object[index])){
        for(var c in object[index]){
          if(distinct_array.indexOf(object[index][c][field].toString()) === -1)
             distinct_array.push(object[index][c][field].toString());
        }
      }else if(Array.isArray(object[index][field])){
        for(var i in object[index][field]){
          if(distinct_array.indexOf(object[index][field][i][id].toString()) === -1)
            distinct_array.push(object[index][field][i][id].toString());
        }
      }else{
        if(typeof object[index][field] !== 'undefined')
          if(_.has(object[index][field], id)){
            if(distinct_array.indexOf(object[index][field][id].toString()) === -1){
              distinct_array.push(object[index][field][id].toString());
            }
          }else{
            if(distinct_array.indexOf(object[index][field]) === -1){
              distinct_array.push(object[index][field]);
            }
          }
      }
    }
    return distinct_array;
  }else{
    return false;
  }
};

Twine.prototype.setContainerResolveResults = function setContainerResolveResults(key,id,cont,results){
  for(var r  in results){
    if(typeof results[r] !== 'undefined')
      results[r] = (typeof results[r].toObject === 'function') ? results[r].toObject() : results[r];
      _logger.log('info', 'results iteration with index '+r+ ': ' + JSON.stringify(results[r]));
      if(typeof results[r][id] !== 'undefined'){
        var res_key = results[r][id].toString();
        _logger.log('info', 'resolve key in setContainerResolveResults', {res_key:res_key});
        cont[key][res_key] = results[r];
        _logger.log('info', 'container set with result index from setContainerResolveResults', {container: cont});
      }
  }
  _logger.log('info', 'returning container from setContainerResolveResults', {container:cont});
  return cont;
};

Twine.prototype.$__ammendResultsWithContainer = function $__ammendResultsWithContainer(container){
  var decision = false;
  var container_keys = Object.keys(container);
  var key = container_keys[0];
  _logger.log("info", "ammendResultsWithContainer keys", {container_keys: container_keys, key:key});
  var add_resolve = false;
  for(var prop in container[key]){
    if(!!prop){
      decision = true;
    }
  }
  _logger.log('info', 'ammendResultsWithContainerKeys decision: '+decision);
  return decision;
};

Twine.prototype.$__buildResolveFetchSelect = function $__buildResolveFetchSelect(model, map_object){
  var select = [];
  if(_.has(map_object, 'format')){
    var format = map_object.format;
    select = model.selectFields(format);
    if(_.has(map_object, 'contains')){
      var contains_keys = Object.keys(map_object.contains);
      if(contains_keys.length > 1){
        for(var key in contains_keys){
          select.push(key);
        }
      }else{
        select.push(contains_keys[0]);
      }
    }
  }
  if(_.keys(map_object, 'fields')){
    var fields = map_object.fields;
    if(_.isArray(fields)){
      for(var i in fields){
        select.push(fields[i]);
      }
    }else if(_.isString(fields)){
      select.push(fields);
    }
  }
  return select.join(" ");
};

Twine.prototype.processResolve = function processResolve(base, map, container, block){
  var self = this;
  //set resolve key
  _logger.log('info', 'map from resolve', {map:map});
  var resolve_field = Object.keys(map)[0];
  //set the object to resolve & unshift the map array
  var resolve_map_object = map[resolve_field];
  _logger.log("info", "resolve mapped object", {resolve_map_object:resolve_map_object});
  //delete the resolve field from the map object
  delete map[resolve_field];
  //get the models can resolve details
  var can_resolve = self.canResolve(resolve_field);
  _logger.log('info','can resolve', {can_resolve:can_resolve});
  //get distinc values for the field to resolve for each field
  var distinct_values = self.getDistinctValuesForField(base.data,
                                                       can_resolve[resolve_field].identifier,
                                                       resolve_field);
  _logger.log('info', 'resolve distinct_values', {distinct: distinct_values});
  //set model to fetch from
  var res_model = _mongoose.model(can_resolve[resolve_field].model);
  //resolve fetch criteria
  var criteria = {};
  criteria[can_resolve[resolve_field].identifier] = {$in : distinct_values};
  _logger.log('info', 'resolve criteria', {criteria: criteria});
  var select = self.$__buildResolveFetchSelect(res_model, resolve_map_object);
  //fetch resolve
  _logger.log('info', 'resolve fetch select', {select:select});
  var fetch_resolve = res_model.find(criteria);
  //set select format
  fetch_resolve.select(select);
  fetch_resolve.exec(function(err, res){
    _logger.log('info', 'fetch resolve results: ', {err:err, results:res});
    //if err, its server related, bubble up and invoke block
    if(err){
      block(err, false);
    }else{
      //check to ensure the model property is set for setting resolved result sets..
      //if not set, ucaught exception is thrown for setting a key for property that doesnt exist.
      if(!_.has(container, can_resolve[resolve_field].model))
        container[can_resolve[resolve_field].model] = {};
      //set container results for resolve key model
      var key = resolve_map_object.model;
      // appliy contains if it exists
      if(_.has(resolve_map_object, 'contains')){
        self.processContains(res, resolve_map_object.contains, function(result){
            container = self.setContainerResolveResults(can_resolve[resolve_field].model,
                                            can_resolve[resolve_field].identifier,
                                            container, result);
            _logger.log('info', 'contains result inside resolve', {result: result, container:container});
            if(self.$__ammendResultsWithContainer(container)) base.resolve = container;
            if(Object.keys(map).length > 0){
              self.processResolve(base, map, container, block);
            }else{
              block(false, base);
            }
        });
      }else if(_.has(resolve_map_object, 'resolve')){
        container = self.setContainerResolveResults(can_resolve[resolve_field].model,
                                                    can_resolve[resolve_field].identifier,
                                                    container, res);
        var fetch_result_base = {data: res};
        self.processResolve(fetch_result_base, resolve_map_object.resolve, container, function(err, base_result){
          if(_.has(base_result, 'resolve')) container = base_result.resolve;
          if(_.keys(map).length > 0){
            self.processResolve(base, map, container, block);
          }else{
            if(self.$__ammendResultsWithContainer(container)) base.resolve = container;
            block(false, base);
          }
        });
      }else{
        //check if map array still has index if so call processResolve
        container = self.setContainerResolveResults(can_resolve[resolve_field].model,
                                        can_resolve[resolve_field].identifier,
                                        container, res);
        if(Object.keys(map).length > 0){
          self.processResolve(base, map, container, block);
        }else{
          if(self.$__ammendResultsWithContainer(container)) base.resolve = container;
          block(false, base);
        }
      }
    }
  });
};
