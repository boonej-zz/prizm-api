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
    User        = require('../models/user').User;

/**
 * Twine Constants
 */
var DEFAULT_LIMIT = 30;
var DEFAULT_SORT_BY = 'create_date';
var DEFAULT_SORT = -1;
var DEFAULT_PAGE_BY = 'create_date';
var DEFAULT_PAGE_DIRECTION = -1;

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
  // if(typeof this.Request.headers['x-arguments'] !== 'undefined'){
  //   var args          = new Buffer(this.Request.headers['x-arguments'], 'base64').toString('utf8');
  //   this.Request.body = JSON.parse(args);
  // }
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
  this.context = null;
  this.filters = {};
  this.model_keys = [];
  this.derived = null;
  this.fetch = null;

  for(var key in this.Schema.paths){
    this.model_keys.push(key);
  }

  self.$__resolveFilterProperties();
  // self.$__resolveDerived();
  self.buildFetchRequest();
}

var doesObjectKeyExist = function(object, key){
  // return (typeof object.key !== 'undefined');
  for(var found in object){
    if(found === key) return true;
  }
  return false;
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
    if(this.Request.body)
      body = (typeof this.Request.body[param] !== 'undefined') ? this.Request.body[param] : null;
    if(this.Request.query)
      qs = (typeof this.Request.query[param] !== 'undefined') ? this.Request.query[param] : null;
    return (body || qs) ? ((qs) ? qs : body) : default_value;
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

Twine.prototype.$__applyResolved = function $__applyResolved(result, cb){
  var distinct = [];
  var resolve_body;
  var resolve_identifier;
  var resolve_model;
  var resolve_format;
  var resolve_model_properties;
  var resolve_model_objects;
  var key;
  var resolve_key;
  var index;
  var queries = [];

  if(this.$__isBodySet() && doesObjectKeyExist(this.Request.body, 'resolve')){
    resolve_body = this.Request.body.resolve;
    resolve_model_objects = this.Model.canResolve();
    if(resolve_model_objects){
      for(var r_idx in resolve_model_objects){
        key = Object.keys(resolve_model_objects[r_idx])[0];

        if(doesObjectKeyExist(this.Request.body.resolve, key) &&
          doesObjectKeyExist(result[0], key)){
          resolve_key = key;

          var rs = result[0][resolve_key];
          resolve_identifier = resolve_model_objects[r_idx][resolve_key].identifier;

          for(var i = 0; i < rs.length; i++){
            distinct.push(rs[i][resolve_identifier].toString());
          }

          if(distinct.length > 0){
            resolve_model = _mongoose.model(resolve_model_objects[r_idx][resolve_key].model);
            queries.push({
              model: resolve_model,
              distinct: distinct,
              resolve_identifier: resolve_identifier,
              resolve_key: resolve_key});
          }
        }

        resolve_model = null;
        key = null;
        resolve_key = null;
        distinct = [];
        resolve_identifier = null;
      }
    }
  }

  var response = {resolve: {}};
  var error = null;

  /**
   * Processes the resolve body options recursively.  Introspects the model
   * object and calls its static method [canResolve] to mitigate between
   * request & capable properties (as well as associated identifier.. sub
   * document arrays can have different object identifiers: ie _id, user_id,
   * creator, etc.).
   *
   * @param {Array} array The request bodys `resolve` object (a tree of opts)
   * @param {Function} callback The callback to be invoked on completion.
   */
  var processResolve = function processResolve(array, callback){
    var criteria = {};
    var object_to_resolve = array.shift();
    criteria[object_to_resolve.resolve_identifier] = {$in: object_to_resolve.distinct};
    //reset model defaulted select
    for(var p in object_to_resolve.model().schema.paths){
      var reset = object_to_resolve.model().schema.paths[p].options.select;
      object_to_resolve.model().schema.paths[p].selected = reset;
    }
    object_to_resolve.model.find(criteria, function(err, res){
      if(err) error = err;
      var fields = (doesObjectKeyExist(resolve_body[object_to_resolve.resolve_key], 'fields')) ?
          resolve_body[object_to_resolve.resolve_key].fields : null;

      //set response key inside root object & set the value to an empty array
      response.resolve[object_to_resolve.resolve_key] = [];
      for(var item in res){
        var format = {};
        // if(typeof res[item][object_to_resolve.resolve_identifier] == 'undefined' &&
        //   res[item][object_to_resolve.resolve_key].length > 0){
        //   console.log(JSON.stringify(res[item][object_to_resolve.resolve_key]));
        //   format[res[item][object_to_resolve.resolve_key][0][object_to_resolve.resolve_identifier].toString()] = res[item].short(fields);
        // }else{
          // if(typeof res[item][object_to_resolve.resolve_identifier] !== 'undefined')
            format[res[item][object_to_resolve.resolve_identifier].toString()] = res[item].short(fields);
        // }
        response.resolve[object_to_resolve.resolve_key].push(format);
      }

      //if there is another value left in the array recursively call itself with current params
      if(array.length > 0){
        processResolve(array, callback);

      }else{
        callback();
      }
      // //check if a format has been set in the child tree of the resolve_key
      // if(doesObjectKeyExist(resolve_body[resolve_key], 'format')){
      //   var fields = (doesObjectKeyExist(resolve_body[resolve_key], 'fields')) ?
      //     resolve_body[resolve_key].fields : null;
      // //otherwise just set the resolve_identifier key and result in response object
      // }else{
      //   response[object_to_resolve.resolve_key] =
      // }
    });
  };

  if(queries.length > 0){
    processResolve(queries, function(){
      //invoke the  callback block
      cb(error, response);
    });

  }else{
    //should throw error
    cb(error, response);
  }
};

Twine.prototype.$__applyContains = function $__applyContains(result, cb){
  var contains_key;
  var contains_identifier;
  var contains_value;
  var contains;

  var unsetContainsArrays = function(object, exception_identifier){
    for(var property in object){
      if(Array.isArray(property) && property !== exception_identifier){
        object[property] = [];
      }
    }
    return object;
  };

  //check if contains property is set in body
  if(doesObjectKeyExist(this.Request.body, 'contains')){
    //set contains lookup property key
    contains_key = Object.keys(this.Request.body.contains)[0]; //TODO: recursively processContains
    //set contains identifier and value
    for(var key in this.Request.body.contains[contains_key]){
      contains_identifier = key;
      contains_value = this.Request.body.contains[contains_key][contains_identifier];
    }

    //loop through the result value and return contains array value if in array
    //or unset the array to empty
    contains = result[0][contains_key];
    var found = false;
    for(var i=0; i < contains.length; i++){
      if(contains[i][contains_identifier].toString() === contains_value)
        result[0][contains_key] = contains[i];
        found = true;
        //var response = unsetContextArray(result[0], )
    }
    if(!found) result[0][contains_key] = [];
    cb(null, result);

  }else{
    //return false
    cb(null, false);
  }
};

/**
 * [buildFetchRequest description]
 * @return {[type]} [description]
 */
Twine.prototype.buildFetchRequest = function buildFetchRequest (){
  //amend the filters to the existing criteria if exists
  if(this.filters){
    for(var key in this.filters){
      this.criteria[key] = this.filters[key];
    }
  }

  //add paging to criteria if exists
  if(this.page){
    if(this.page_direction === 1){
      this.criteria[page_by] = {$lt : this.page};
    }else if(this.page_direction === -1){
      this.criteria[page_by] = {$gt : this.page};
    }else{
      this.criteria[page_by] = {$gt : this.page};
    }
  }

  //init fetch query object with fetch criteria;
  this.fetch = this.Model.find(this.criteria);

  //add selection fields if exists
  var fields_internal;
  if(this.fields){
    //this may need to be done on result
    //this.fetch.select(this.fields);

    var fields_array = this.fields.split(" ");
    fields_internal = this.Model.selectFields('basic');
    for(var field in fields_array){
      if(fields_internal.indexOf(fields_array[field]) === -1){
        fields_internal.push(fields_array[field]);
      }
    }
  }else{
    fields_internal = this.Model.selectFields('basic');
  }

  //this.fetch.select(fields_internal.join(" "));

  //debugger;

  //ensure if properties exist in resolve body that they are set
  //to be selected on the Schema path object
  if(doesObjectKeyExist(this.Request.body, 'resolve')){
    var keys = Object.keys(this.Request.body.resolve);
    if(Array.isArray(keys) && keys.length > 0){
      for(var idx in keys){
        if(fields_internal.indexOf(keys[idx]) === -1){
          fields_internal.push(keys[idx]);
        }
      }
    }
  }

  //same for contains
  if(doesObjectKeyExist(this.Request.body, 'contains')){
    var keys = Object.keys(this.Request.body.contains);
    if(Array.isArray(keys) && keys.length > 0){
      for(var idx in keys){
        if(fields_internal.indexOf(keys[idx]) === -1){
          fields_internal.push(keys[idx]);
        }
      }
    }
  }

  this.fetch.select(fields_internal.join(" "));

  //add sort if fields exist
  if(this.sort && this.sort_by){
    var sort = {};
    //add default sort (if the passed sort is not create -- we always want the
    //the most recent post first)
    if(this.sort_by !== DEFAULT_PAGE_BY) sort[DEFAULT_PAGE_BY] = DEFAULT_PAGE_DIRECTION;
    sort[this.sort_by] = this.sort;
    this.fetch.sort(sort);
  }

  //TODO: add getter & setter for default limit
  //set the limit -- default limit is set to 30
  this.fetch.limit(this.limit);

  var self = this;
  this.fetch.exec(function(err, result){
    var response = {};
    self.$__applyResolved(result, function(err, resolved){
      if(err){
        self.cb(err, null);

      }else{
        //set derived if exists
        if(Object.keys(resolved.resolve).length > 0) response = resolved;

        //apply context
        self.$__applyContains(result, function(err, contains){
          if(err){
            self.cb(err, null);

          }else{
            //HACK: if resolved is set the previous the only resolved object that needs
            //to be returned is the one specified in the contains
            if(doesObjectKeyExist(response, 'resolve')){
              if(contains && Object.keys(response.resolve).length > 0){
                var contains_key = Object.keys(self.Request.body.contains)[0];
                if(doesObjectKeyExist(self.Request.body.resolve, contains_key)){
                  var identifier_to_check = Object.keys(self.Request.body.contains[Object.keys(self.Request.body.contains)[0]])[0];
                  for(var r in resolved.resolve[contains_key]){
                    if(doesObjectKeyExist(resolved.resolve[contains_key][r], self.Request.body.contains[contains_key][identifier_to_check])){
                      response.resolve[contains_key] = [resolved.resolve[contains_key][r]];
                      break;

                    }
                  }
                }
              }
            }
            //format return object
            response.data = (!contains) ? result : contains;
            self.cb(null, response);
          }
        });
      }
    });
  });
};

