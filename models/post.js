/**
 * Post Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose'),
    _serial     = require('serializer'),
    _crypt      = require('crypto'),
    _utils      = require(process.env.PRISM_HOME + 'utils'),
    User        = require(process.env.PRISM_HOME + 'models/user').User;

/**
 * Comment Model Schema
 * @type {Mongoose.Schema}
 */
var commentSchema = new _mongoose.Schema({
  text                : { type: String, default: null, required: true },
  creator             : { type: _mongoose.Schema.Types.ObjectId, ref: 'User'},
  create_date         : { type: Date, default: Date.now() },
  likes               : [],
  likes_count         : {type: Number, default: 0}
});

commentSchema.statics.selectFields = function(type){
  var select = ['comments._id', 'comments.text',
    'comments.creator','comments.create_date','comments.likes_count'];
  if(type === 'basic')
    select.push('comments.likes');
  return select;
};

commentSchema.statics.canResolve = function(){
  return [
    {creator: {identifier: '_id', model: 'User'}},
    {likes: {identifier: '_id', model: 'User'}}
  ];
};

/**
 * Description of Post Status types
 * @type {Object}
 */
var status_types = {
  active:   'active',
  deleted:  'deleted',
  review:   'review'
};

/**
 * Post Model Schema
 * @type {Mongoose.Schema}
 */
var postSchema = new _mongoose.Schema({
  text                : {type: String, default: null},
  category            : {type: String, required:true},
  create_date         : {type: Date, default:null, index: true},
  modify_date         : {type: Date, default: Date.now()},
  delete_date         : {type: Date, default: null},
  scope               : {type: String, default: 'public'},
  location_name       : {type: String, default: null},
  location_longitude  : {type: Number, default: 0},
  location_latitude   : {type: Number, default: 0},
  creator             : {type: _mongoose.Schema.Types.ObjectId, ref: 'User'},
  target_id           : {type: String, required: true},
  status              : {type: String, default: 'active'},
  file_path           : {type: String, default: ''},
  likes_count         : {type: Number, default: 0},
  comments_count      : {type: Number, default: 0},
  tags_count          : {type: Number, default: 0},
  tags                : {type: Array, default: []},
  comments            : [commentSchema],
  likes               : {type: Array, default: []},
  hash_tags           : [String],
  hash_tags_count     : {type: Number, default: 0},
  is_flagged          : {type: Boolean, default: false},
  flagged_count       : {type: Number, default: 0},
  flagged_reporters   : [{reporter_id: String, create_date: Date}],
  is_repost           : {type: Boolean, default: false},
  origin_post_id      : {type: String, default: null},
  external_provider   : {type: String, default: null},
  external_link       : {type: String, default: null},
  type                : {type: String, default: 'user'}
}, { versionKey: false});

postSchema.statics.canResolve = function(){
  return [
    {creator: {identifier: '_id', model: 'User'}},
    {target_id: {identifier: '_id', model: 'User'}},
    {comments: {identifier: 'creator', model: 'User'}},
    {likes: {identifier: '_id', model: 'User'}},
    {origin_post_id: {identifier: '_id', model: 'Post'}}
  ];
};

postSchema.statics.selectFields = function(type){
  if(type === 'short'){
    return ['_id','text','category','create_date','file_path',
            'location_name','location_longitude','location_latitude',
            'creator','target_id','likes_count','comments_count','scope',
            'hash_tags','hash_tags_count'];
  }else{
    return ['_id','text','category','create_date','file_path',
            'location_name','location_longitude','location_latitude',
            'creator','target_id','likes_count','comments_count','scope',
            'status','hash_tags','hash_tags_count','is_repost','origin_post_id','modify_date',
            'delete_date'];
  }
};

postSchema.methods.parseAndUpdateTags = function(){
  var parsed = [];
  parsed= this.text.match(/(@(\S+))/g);
  if(parsed.length > 0){
    for(var i = 0; i < parsed.length; i++){
      parsed[i] = parsed[i].replace(/@/, "");
      if(this.tags.length === 0)
        this.tags.push({_id: parsed[i]});
      if(this.tags.indexOf(parsed[i]) === -1)
        this.tags.push({_id: parsed[i]});
    }
  }
};

postSchema.methods.format = function(type, add_fields){
  var format;
  if(!type) type = 'basic';

  format = {
    _id:                  this._id,
    text:                 this.text,
    category:             this.category,
    create_date:          this.create_date,
    file_path:            this.file_path,
    location_name:        this.location_name,
    location_longitude:   this.location_longitude,
    location_latitude:    this.location_latitude,
    creator:              this.creator,
    target_id:            this.target_id,
    likes_count:          this.likes_count,
    comments_count:       this.comments_count,
    hash_tags_count:      this.hash_tags_count,
    hash_tags:            this.hash_tags,
    scope:                this.scope
  };

  if(type === 'basic'){
    format.status         = this.status;
    format.is_repost      = this.is_repost;
    format.origin_post_id = this.origin_post_id;
    format.modify_date    = this.modify_date;
    format.delete_date    = this.delete_date;
  }

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

/**
 * validates category property
 * @param  {[type]} value [description]
 * @return {[type]}       [description]
 */
postSchema.path('category').validate(function(value){
  value.toLowerCase();
  value = value.charAt(0).toUpperCase() + value.slice(1);
  return /Aspirations|Aspiration|Passions|Passion|Experiences|Experience|Achievements|Achievement|Inspirations|Inspiration|Personal|Personals/i.test(value);
}, 'Invalid Category Type');

/**
 * validates scope property
 * @param  {[type]} value [description]
 * @return {[type]}       [description]
 */
// postSchema.path('scope').validate(function(value){
//   value.toLowerCase();
//   return /private|public/i.test(value);
// }, 'Invalid Scope Type');

postSchema.methods.flagPostValidation = function(){
  if(this.flagged_reporters.length !== this.flagged_count){
    this.flagged_count = this.flagged_reporters.length;
  }

  if(this.is_flagged === false && this.flagged_reporters.length > 0){
    this.is_flagged = true;
  }

  if(this.flagged_reporters.length >= 5 && this.status !== 'review'){
    this.status = status_types.review;
  }
};

postSchema.methods.updateFields = function(){
  return ['text', 'category', 'filepath', 'scope', 'location_name',
          'location_longitude', 'location_latitude'];
};

postSchema.methods.fetchRepostShortUser = function(post_id, cb){
  this.model('Post').findOne({_id: post_id}, function(err, org){
    if(err) throw err;
    User.findOne({_id: org.creator}, function(err, user){
      cb(err, user.shortUser());
    });
  });
};

/**
 * Pre Save/Creation Injection
 *
 * Sets the modify_date anytime the record is saved.
 * If its the first time the record is saved, the create_date
 * is date stamped
 *
 * @param  {Function} next Calls the next() iterator to continue process
 */
postSchema.pre('save', function(next){
	//set create & modify dates
	this.modify_date = Date.now();
  if(!this.create_date){
    if(this.create_date === null) this.create_date = Date.now();
	}

  //check that counts are accurate to arrays, if not increment there values
  if(typeof(this.likes) !== 'undefined'){
    if(this.likes.length !== this.likes_count) this.likes_count = this.likes.length;
  }

  if(typeof(this.comments) !== 'undefined'){
    if(this.comments.length !== this.comments_count) this.comments_count = this.comments.length;
  }

  if(typeof(this.flagged_reporters) !== 'undefined'){
    if(this.flagged_reporters.length > 0) this.flagPostValidation();
  }

  this.parseAndUpdateTags();
  if(this.tags > 0){
    this.tags_count = this.tags.length;
  }

	next();
});

/**
 * Pre Update Injection
 *
 * Date stampes the modify_date field
 *
 * @param  {Function} next Calls the next() iterator to continue process
 */
postSchema.pre('update', function(next){
	this.modify_date = Date.now();
  this.parseAndUpdateTags();
	next();
});

exports.Post    = _mongoose.model('Post', postSchema);
exports.Comment = _mongoose.model('Comment', commentSchema);
