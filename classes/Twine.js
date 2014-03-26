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

Twine.prototype.$__applyDerived = function $__applyDerived(result, cb){
  var distinct = [];
  var derived_body;
  var derived_identifier;
  var derived_model;
  var derived_format;
  var derived_model_properties;
  var derived_model_objects;
  var key;
  var index;

  if(this.$__isBodySet() && doesObjectKeyExist(this.Request.body, 'derived')){
    derived_body = this.Request.body.derived;
    derived_model_objects = this.Model.derived();
    if(derived_model_objects){
      for(var derived in derived_model_objects){
        derived_key = Object.keys(derived_model_objects[derived])[0];

        if(doesObjectKeyExist(this.Request.body.derived, derived_key) &&
          doesObjectKeyExist(result[0], derived_key)){
          key = derived_key;

          var rs = result[0][derived_key];
          derived_identifier = derived_model_objects[derived][derived_key].identifier;

          for(var i = 0; i < rs.length; i++){
            distinct.push(rs[i][derived_identifier].toString());
            /**
            if(distinct.length === 0){
              distinct.push(rs[i][derived_identifier].toString());
            }else if(distinct.indexOf(rs[i][derived_identifier].toString()) !== -1){
              distinct.push(rs[i][derived_identifier].toString());
            }
            */
          }

          if(distinct.length > 0)
            derived_model = _mongoose.model(derived_model_objects[derived][derived_key].model);
          continue;
        }
      }
    }
  }

  if(distinct.length > 0 && derived_identifier && derived_model){
    var criteria = {};
    criteria[derived_identifier] = {$in: distinct};
    derived_model.find(criteria, function(err, res){
      if(err){
        cb(err, false);

      }else{
        debugger;
        if(doesObjectKeyExist(derived_body[key], 'format')){
          var response = {derived: []};
          var fields = (doesObjectKeyExist(derived_body[key], 'fields')) ?
            derived_body[key].fields : null;

          for(var d in res){
            var format = {};
            format[res[d]._id.toString()] = res[d].short(fields);
            response.derived.push(format);
          }
          cb(null, response);

        }else{
          cb(false);
        }
      }
    });
  }else{
    cb(false);
  }
};

Twine.prototype.$__applyContext = function $__applyContext(){

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
  if(this.fields){
    //this may need to be done on result
    this.fetch.select(this.fields);
  }

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
  debugger;

  var self = this;
  this.fetch.exec(function(err, result){
    var response = {};
    self.
    self.$__applyDerived(result, function(err, derived){
      if(err){
        self.cb(err, null);
      }else{
        //set derived if exists
        if(derived) response = derived; 

        //apply context
        self.$__applyContext(result, function(err, context){
          if(err){
            self.cb(err, null);
          }
        });
      }
    });
  });

};

// var criteria = User.find({_id: 'asdfasdfasdf'});
// new Twine('User', {first_name: 'dj', scope: 'active'}, {Request: 'asdf'}, null, function(err, result){

// });











