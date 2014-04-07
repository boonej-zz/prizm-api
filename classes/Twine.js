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
  this.model_keys = [];
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
  if(this.Request && doesObjectKeyExist(this.Request.headers, X_ARGUMENTS_HEADER)){
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
};

Twine.prototype.$__setPage = function $__setPage(cb){};
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
    var array = this.fields.split(" ");
    for(var a in array){
      if(select.indexOf(array[a]) === -1) select.push(array[a]);
    }
  }

  this.fetch.select(select.join(" "));
};
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

Twine.prototype.buildBaseRequest = function buildBaseRequest (){
  //set intial fetch object with base criteria;
  this.fetch = this.Model.find(this.criteria);
  //ammend fetch with page, sort, filter, & select fields
  this.$__setSelect();
  //only execute the request if the callback is set. if not
  //the executerequest can take an optional cb -- to invoked seperately
  //TODO: worry about this on second pass.. just call for now
  //if(this.cb) this.executeRequest();
  this.executeRequest();
};

Twine.prototype.executeRequest = function executeRequest (){
  var response = {};
  var self = this;
  this.fetch.exec(function(err, result){
    //if there is an error returned from the fetch its server related
    //so send it back immediately
    if(err) this.cb(err, null);
    //if the result set is empty, invoke callback immediately, else process
    if(result.length === 0){
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

Twine.prototype.process = function process (base, block){
  var container = {};
  var self = this;
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
    var k = (Array.isArray(contains))? Object.keys(contains[contain])[0] : contain;
    //set contain value
    var v = (Array.isArray(contains))? contains[contain][k] : contains[contain];
    //loop through each array in the result set & compare to contains key, value
    _logger.log('info', 'contain loop key value', {contain:contain, contains:contains, key:k, value:v});
    for(var num in base.data){
      var found = false;
      var has_key = false;
      if(doesObjectKeyExist(base.data[num], contain)){
        has_key = true;
        for(var check in base.data[num][contain]){
          _logger.log('info', 'does base.data index ' + num + ' field ' +contain+ ' field index + '+check+' key + '+k+' == value: '+v);
          if(base.data[num][contain][check][k].toString() === v){
            _logger.log('info', 'contains key value found: ' +base.data[num][contain][check][k].toString()+ ' matching v value: '+ v);
            base.data[num][contain] = [v];
            found = true;

          }
        }
        if(!found && has_key) base.data[num][contain] = [];
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
    if(Object.keys(model_resolve_fields[i])[0] === field) return model_resolve_fields[i];
  }
};

Twine.prototype.getDistinctValuesForField = function getDistinctValuesForField(object,id,field){
  var distinct_array = [];
  if(Array.isArray(object)){
    for(var index in object){
      object[index] = (typeof object[index].toObject === 'function')? object[index].toObject() : object[index];
      if(Array.isArray(object[index][field])){
        for(var i in object[index][field]){
          if(distinct_array.indexOf(object[index][field][i][id].toString()) === -1)
            distinct_array.push(object[index][field][i][id].toString());
        }
      }else{
        if(distinct_array(object[index][field][id].toString()) === -1)
          distinct_array.push(object[index][field][id].toString());
      }
    }
    return distinct_array;
  }else{
    return false;
  }
};

Twine.prototype.setContainerResolveResults = function setContainerResolveResults(key,id,cont,results){
  for(var r  in results){
    results[r] = (typeof results[r].toObject === 'function')? results[r].toObject() : results[r];
      var res_key = results[r][id].toString();
      cont[key][res_key] = results[r];
  }
  return cont;
};

Twine.prototype.processResolve = function processResolve(base, map, container, block){
  var self = this;
  //set resolve key
  var resolve_field = Object.keys(map)[0];
  //set the object to resolve & unshift the map array
  var resolve_map_object = map[resolve_field];
  //delete the resolve field from the map object
  delete map[resolve_field];
  //get the models can resolve details
  var can_resolve = self.canResolve(resolve_field);
  //get distinc values for the field to resolve for each field 
  var distinct_values = self.getDistinctValuesForField(base.data, 
                                                       can_resolve[resolve_field].identifier, 
                                                       resolve_field);
  //set model to fetch from
  var res_model = _mongoose.model(can_resolve[resolve_field].model);
  //resolve fetch criteria
  var criteria = {};
  criteria[can_resolve[resolve_field].identifier] = {$in : distinct_values};
  //fetch resolve
  res_model.find(criteria, function(err, res){
    //if err, its server related, bubble up and invoke block
    if(err){
      block(err, false);
    }else{
      //set container results for resolve key model 
      var key = resolve_map_object.model;
      var format = resolve_map_object.format;
      // if(doesObjectKeyExist(resolve_map_object, 'contains')){
      //   self.processContains(res, resolve_map_object.contains, function(err, result){
      //     if(err){
      //       block(err, false);
      //     }else{
      //       self.setContainerResolveResults(key, can_resolve.identifier, container, res);
      //       if(Object.keys(map).length > 0){
      //         self.processResolve(base, map, container, block);
      //       }else{
      //         block(false, container);
      //       }
      //     }
      //   });
      // }else{
        //check if map array still has index if so call processResolve
        if(!doesObjectKeyExist(container, can_resolve[resolve_field].model)) 
          container[can_resolve[resolve_field].model] = {};
        container = self.setContainerResolveResults(can_resolve[resolve_field].model, 
                                        can_resolve[resolve_field].identifier, 
                                        container, res);
        if(Object.keys(map).length > 0){
          self.processResolve(base, map, container, block);
        }else{
          debugger;
          if(Object.keys(container[Object.keys(container)[0]]).length > 0) base.resolve = container;
          block(false, base);
        }
      // }
      }
    });
};
