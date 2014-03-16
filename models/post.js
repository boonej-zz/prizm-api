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
  tags                : [],
  comments            : [commentSchema],
  likes               : [],
  hash_tags           : [],
  hash_tags_count     : {type: Number, default: 0},
  is_flagged          : {type: Boolean, default: false},
  flagged_count       : {type: Number, default: 0},
  flagged_reporters   : [{reporter_id: String, create_date: Date}],
  is_repost           : {type: Boolean, default: false},
  origin_post_id      : {type: String, default: null}
}, { versionKey: false});

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
	next();
});

exports.Post    = _mongoose.model('Post', postSchema);
exports.Comment = _mongoose.model('Comment', commentSchema);
