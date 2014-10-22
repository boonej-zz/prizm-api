/**
 * Insight Model
 * Jonathan Boone
 */

var mongoose     = require('mongoose'),
    utils        = require(process.env.PRISM_HOME + 'utils'),
    User          = require(process.env.PRISM_HOME + 'models/user').User,
    Trust         = require(process.env.PRISM_HOME + 'models/user').Trust,
    Post          = require(process.env.PRISM_HOME + 'models/post').Post,
    ObjectId    = mongoose.Schema.Types.ObjectId;

var insightSchema = new mongoose.Schema({
  creator         : {type: ObjectId, ref: 'User', required: true},
  create_date     : {type: Date, default: null, required: false, index: true},
  title           : {type: String, default: null, required: true},
  text            : {type: String, default: null, required: true}, 
  file_path       : {type: String, default: ''},
  likes_count     : {type: Number, default: 0},
  dislikes_count  : {type: Number, default: 0},
  tags_count      : {type: Number, default: 0},
  tags            : {type: Array, default: []},
  hash_tags       : {type: [String], default: []},
  hash_tags_count : {type: Number, default: 0},
  link            : {type: String, default: null},
  link_title      : {type: String, default: null}
}); 

insightSchema.statics.selectFields = function(type){
  var select = ['_id', 'text', 'creator','create_date','likes_count',
      'dislikes_count', 'tags_count', 'tags', 'hash_tags', 'hash_tags_count',
      'file_path', 'title', 'link', 'link_title'];
  return select;
};

insightSchema.methods.format = function(type, add_fields){
  var format;
  if(!type) type = 'basic';

  format = {
    _id:                  this._id,
    creator:                 this.text,
    create_date:          this.create_date,
    file_path:            this.file_path,
    likes_count:          this.likes_count,
    dislikes_count:       this.dislikes_count,
    hash_tags_count:      this.hash_tags_count,
    hash_tags:            this.hash_tags,
    tags:                 this.tags,
    title:                this.title,
    tags_count:           this.tags_count,
    link:                 this.link,
    link_title:           this.link_title
  };

  if(add_fields){
    if(typeof add_fields === 'string') format[add_fields] = this[add_fields];
    if(Array.isArray(add_fields) && add_fields.length > 0){
      for(var idx in add_fields){
        format[add_fields[idx]] = this[add_fields[idx]];
      }
    }
  }
  return format;
};
var insightTargetSchema = new mongoose.Schema({
  create_date     : {type: Date, default: null, required: false, index: true},
  insight         : {type: ObjectId, ref: 'Insight', required: true},
  creator         : {type: ObjectId, ref: 'User', required: true},
  target          : {type: ObjectId, ref: 'User', required: true},
  liked           : {type: Boolean, default: false},
  disliked        : {type: Boolean, default: false},
  file_path       : {type: String, default: null}
});

insightSchema.pre('save', function(next){
  this.create_date = Date.now();
  next();
});

insightTargetSchema.pre('save', function(next){
  this.create_date = Date.now();
  next();
});

insightTargetSchema.statics.selectFields = function(type){
  var select = ['_id', 'create_date', 'insight','target','liked',
      'disliked', 'creator', 'file_path'];
  return select;
};

insightTargetSchema.statics.canResolve = function(){
  return [
    {target: {identifier: '_id', model: 'User'}},
    {insight: {identifier: '_id', model: 'Insight'}},
    {creator: {identifier: '_id', model: 'User'}}
  ];
};


insightSchema.methods.format = function(type, add_fields){
  var format;
  if(!type) type = 'basic';

  format = {
    _id:                  this._id,
    target:               this.target,
    create_date:          this.create_date,
    insight:              this.insight,
    liked:                this.liked,
    disliked:             this.disliked,
    creator:              this.creator,
    file_path:            this.file_path
  };

  if(add_fields){
    if(typeof add_fields === 'string') format[add_fields] = this[add_fields];
    if(Array.isArray(add_fields) && add_fields.length > 0){
      for(var idx in add_fields){
        format[add_fields[idx]] = this[add_fields[idx]];
      }
    }
  }
  return format;
};

mongoose.model('Insight', insightSchema);
mongoose.model('InsightTarget', insightTargetSchema);
