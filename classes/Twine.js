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
    User        = require('../models/user').User;

/**
 * Twine Constants
 */
var DEFAULT_LIMIT = 30;
var DEFAULT_SORT_BY = 'create_date';
var DEFAULT_SORT = -1;
var DEFAULT_PAGE_BY = 'create_date';
var DEFAULT_PAGE_DIRECTION = -1;
var X_ARGUMENTS_HEADER = 'x-arguments';

/**
 * Expose 'Twine'
 */
module.exports = Twine;

function Twine(model, criteria, Request, options, callback){
  var self = this;
  this.Model = _mongoose.model(model);
  this.Schema = this.Model().schema;
  this.criteria = criteria;
  this.Request = Request;
  this.args = self.$__digestHeaderArguments();
  this.options = options;
  this.cb = callback;
  this.sort = (!self.$__optExists('sort')) ?
    self.$__parse('sort', DEFAULT_SORT)
    : options.sort;
  this.sort_by = (!self.$__optExists('sort_by')) ?
    self.$__parse('sort_by', DEFAULT_SORT_BY)
    : options.sort_by;
  this.limit = (!self.$__optExists('limit')) ?
    self.$__parse('limit', DEFAULT_LIMIT)
    : options.limit;
  this.fields = (!self.$__optExists('fields')) ?
    self.$__parse('fields', null)
    : options.fields;
  this.page = (!self.$__optExists('page')) ?
    self.$__parse('page', null)
    : options.page;
  this.page_by = (!self.$__optExists('page_by')) ?
    self.$__parse('page_by', DEFAULT_PAGE_BY)
    : options.page_by;
  this.page_direction = (!self.$__optExists('page_direction')) ?
    self.$__parse('page_direction', DEFAULT_PAGE_DIRECTION)
    : options.page_direction;
  this.filters = {};
  this.contains = (!self.$__optExists('contains')) ? 
    self.$__parse('contains', null)
    : options.contains;
  this.resolve = (!self.$__optExists('resolve')) ?
    self.$__parse('resolve', null)
    : options.resolve;
  this.fetch = null;

  //intro the Schema & set available model keys
  for(var key in this.Schema.paths){
    this.model_keys.push(key);
  }

  self.$__resolveFilterProperties();
  self.buildBaseRequest();
}

var doesObjectKeyExist = function(object, key){
  // return (typeof object.key !== 'undefined');
  for(var found in object){
    if(found === key) return true;
  }
  return false;
};

Twine.prototype.$__digestHeaderArguments = function $__digestHeaderArguments(){
  var res = null;
  if(this.Request && typeof this.Request.headers[X_ARGUMENTS_HEADER] !== 'undefined'){
    //base64 decode the x-args header
    var args = new Buffer(this.Request.headers[X_ARGUMENTS_HEADER], 'base64').toString('utf8');
    res = JSON.parse(args);
  }
  return res;
};

Twine.prototype.$__optExists = function $__optExists(option){
    if(!this.options){
      return false;
    }else if(doesObjectKeyExist(this.options, option)){
      return true;
    }else{
      return false;
    }
};

Twine.prototype.$__isSchemaProperty = function $__isSchemaProperty(property){
  for(var prop in this.Schema.paths){
    if(prop === property) return true;
  }
  return false;
};

Twine.prototype.$__isQuerySet = function $__isQuerySet(){
  return (doesObjectKeyExist(this.Request, 'query'));
};

Twine.prototype.$__isBodySet = function $__isBodySet(){
  return (doesObjectKeyExist(this.Request, 'body'));
};

Twine.prototype.$__parse = function $__parse(param, default_value){
    var body = null;
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
};

Twine.prototype.$__setPage = function $__setPage(cb){};
Twine.prototype.$__setSelect = function $__setSelect(cb){};
Twine.prototype.$__setSort = function $__setSort(cb){};
Twine.prototype.$__setLimit = function $__setLimit(cb){};
Twine.prototype.$__setFilters = function $__setFilters(cb){
  //amend the filters to the existing criteria if exists
  if(this.filters){
    for(var key in this.filters){
      this.criteria[key] = this.filters[key];
    }
  }
};

Twine.prototype.buildBaseRequset = function buildBaseRequest (){
  //set intial fetch object with base criteria;
  this.fetch = this.Model.find(this.criteria);
  //ammend fetch with page, sort, filter, & select fields
  //TODO: add after we get what we are initially looking for.
  //**set a basic selectFields for now
  this.fetch.select(this.Model.selectFields('basic').join(' '));

  //only execute the request if the callback is set. if not
  //the executerequest can take an optional cb -- to invoked seperately
  //TODO: worry about this on second pass.. just call for now
  //if(this.cb) this.executeRequest();
  this.executeRequest();
};

Twine.prototype.executeRequest = function executeRequest (){
  var response = {};
  this.fetch.exec(function(err, result){
    //if there is an error returned from the fetch its server related
    //so send it back immediately
    if(err) this.cb(err, null);
    //if the result set is empty, invoke callback immediately, else process
    if(result.length === 0){
      this.cb(err, result);
    }else{
      //set the base result set in the response object
      response.data = result;
      //if contains or resolve properties are set process, else return response
      if(!this.contains || !this.resolve){
        this.cb(err, response);
      }else{
        this.process(response, function(err, process_result){
          this.cb(err, process_result);
        });
      }
    }
  });
};

Twine.prototype.process = function process (base, block){
  var container = {};
  //if contains does exist send to resolve -- if nothing to resolve it will invoke 
  //the callback `block` and finish the process, thus retuning the result
  if(!this.contains){
    this.processResolve(base, this.resolve, container, block);
  }else{
    this.processContains(base, this.contains, function(base){
      //bubble up error?
      this.processResolve(base, this.resolve, container, block);
    });
  }
};

Twine.prototype.processContains = function processContains(base, contains, block){
  for(var contain in contains){
    //set contain key
    var k = Object.keys(contains[contain])[0];
    //set contain value
    var v = contains[contain][k];
    //loop through each array in the result set & compare to contains key, value
    for(var num in base){
      if(doesObjectKeyExist(base[num], contain)){
        for(var check in base[num][contain]){
          if(base[num][contain][check][k] === v) base[num][contain] = [v];
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
  var model_resolve_fields = this.Model.canResolve();
  for(var i in model_resolve_fields){
    if(model_resolve_fields[i] === field) return model_resolve_fields[i];
  }
};

Twine.prototype.getDistinctValuesForField = function getDistinctValuesForField(object, field){
  var distinct_array = [];
  if(Array.isArray(object)){
    for(var index in object){
      if(distinct_array.indexOf(object[index][field]) === -1)
        distinct_array.push(object[index][field]);
    }
    return distinct_array;
  }else{
    return false;
  }
};

Twine.prototype.setContainerResolveResults = function setContainerResolveResults(key,id,cont,results){
  for(var r  in results){
    cont[key][results[r][id]] = results[r];
  }
};

Twine.prototype.processResolve = function processResolve(base, map, container, block){
  //set the object to resolve & unshift the map array
  var resolve_map_object = map.shift();
  //set resolve key
  var resolve_field = Object.keys(resolve_map_object)[0];
  //get the models can resolve details
  var can_resolve = this.canResolve(resolve_field);
  //get distinc values for the field to resolve for each field 
  var distinct_values = getDistinctValuesForField(base, resolve_field);
  //set model to fetch from
  var res_model = _mongoose.model(can_resolve[resolve_field].model);
  //resolve fetch criteria
  var criteria = {};
  criteria[resolve_field] = {$in : distinct_values};
  //fetch resolve
  res_model.find(criteria, function(err, res){
    //if err, its server related, bubble up and invoke block
    if(err){
      block(err, false);
    }else{
      //set container results for resolve key model 
      var key = resolve_map_object[resolve_field].model;
      var format = resolve_map_object[resolve_field].format;
      if(doesObjectKeyExist(resolve_map_object, 'contains')){
        processContains(res, resolve_map_object.contains, function(err, result){
          if(err){
            block(err, false);
          }else{
            this.setContainerResolveResults(key, can_resolve.identifier, container, res);
            if(mapy.length > 0){
              this.processResolve(base, map, container, block);
            }else{
              block(false, container);
            }
          }
        });
      }else{
        //check if map array still has index if so call processResolve 
        if(map.length > 0){
          this.processResolve(base, map, container, block);
        }else{
          block(false, container);
        }
      }
    }
  });
};
