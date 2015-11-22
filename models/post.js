/**
 * Post Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose'),
    _serial     = require('serializer'),
    _crypt      = require('crypto'),
    _           = require('underscore'),
    _moment     = require('moment'),
    _utils      = require(process.env.PRISM_HOME + 'utils'),
    User        = require(process.env.PRISM_HOME + 'models/user').User;
var ObjectId = _mongoose.Schema.Types.ObjectId;
var mObjectId = _mongoose.Types.ObjectId;
var time       = require('../lib/helpers/date_time');

/**
 * Comment Model Schema
 * @type {Mongoose.Schema}
 */
var commentSchema = new _mongoose.Schema({
  text                : { type: String, default: null, required: true },
  creator             : { type: _mongoose.Schema.Types.ObjectId, ref: 'User'},
  create_date         : { type: Date, default: Date.now() },
  likes               : [],
  likes_count         : {type: Number, default: 0},
  tags                : {type: Array, default: []},
  hash_tags           : {type: Array, default: []},
  tags_count          : {type: Number, default: 0},
  hash_tags_count     : {type: Number, default: 0},
  status              : {type: String, default: 'active'}
});

commentSchema.methods.format = function(type, add_fields){
  var format;
  if(!type) type = 'basic';

  format = {
    _id             : this._id,
    creator         : this.creator,
    create_date     : this.create_date,
    likes           : this.likes,
    likes_count     : this.likes_count,
    tags            : this.tags,
    hash_tags       : this.hash_tags,
    tags_count      : this.tags_count,
    hash_tags_count : this.hash_tags_count,
    status          : this.status
  };

}
commentSchema.methods.sendTagActivityEvent = function(user_id){
  var from_user, to_user, post_id;

  to_user   = user_id.toString();

  if(_.isUndefined(this.creator._id)){
    from_user = this.creator.toString();
  }else{
    from_user = this.creator._id.toString();
  }

  comment_id = this._id.toString();

  //register tagged activity event
  _utils.registerActivityEvent(to_user, from_user, 'tag', null, comment_id);
};


commentSchema.statics.selectFields = function(type){
  var select = ['comments._id', 'comments.text',
    'comments.creator','comments.create_date','comments.likes_count', 'comments.tags', 'comments.hash_tags', 'comments.tags_count', 'comments.hash_tags_count'];
  if(type === 'basic')
    select.push('comments.likes');
  return select;
};

commentSchema.statics.canResolve = function(){
  return [
    {creator: {identifier: '_id', model: 'User'}},
    {likes: {identifier: '_id', model: 'User'}},
    {tags: {identifier: '_id', model: 'User'}}
  ];
};

commentSchema.methods.parseAndUpdateTags = function(){
  var user_tage = [], hash_tag = [];

  if(this.text){
    user_tag = this.text.match(/(@(\S+))/g);
    hash_tag = this.text.match(/(#(\S+))/g);
    this.tagHandler(ParseTagType.UserTag, user_tag);
    this.tagHandler(ParseTagType.HashTag, hash_tag);
  }
};

commentSchema.methods.tagHandler = function(type, parsed_array){
  if(_.isArray(parsed_array)){
    if(parsed_array.length > 0){
      if(type === ParseTagType.HashTag){
        for(var idx in parsed_array){
          //strip # character from tag
          parsed_array[idx] = parsed_array[idx].replace(/#/, "");
        }
        this.hash_tags = parsed_array;
        this.hash_tags_count = this.hash_tags.length;
      }
      if(type === ParseTagType.UserTag){
        for(var i = 0; i < parsed_array.length; i++){
          //strip the @ character from tag
          parsed_array[i] = parsed_array[i].match(/([a-zA-Z0-9]+)/)[0];
          var user_id = parsed_array[i].replace(/@/, "");
          var tag_added = false;

          //ensure parsed identifier has the structure of a hex string
          var checkForHexObjectId = new RegExp("^[0-9a-fA-F]{24}$");
          if(checkForHexObjectId.test(user_id)){
            if(this.tags.length === 0){
              this.tags.push({_id: user_id});
              tag_added = true;

            }else{
              var item = function(i){
                return i._id === user_id;
              };

              if(_.filter(this.tags, item).length === 0){
                this.tags.push({_id: user_id});
                tag_added = true;
              }
            }
          }

          if(tag_added) this.sendTagActivityEvent(user_id);
        }
      }
    }
  }
};

commentSchema.pre('save', function(next){
  //set create & modify dates

  //check that counts are accurate to arrays, if not increment there values
  if(typeof(this.likes) !== 'undefined'){
    if(this.likes.length !== this.likes_count) this.likes_count = this.likes.length;
  }

  this.parseAndUpdateTags();
  if(this.tags > 0){
    this.tags_count = this.tags.length;
  }

  next();
});



/**
 * Description of Post Status types
 * @type {Object}
 */
var status_types = {
  active:   'active',
  deleted:  'deleted',
  review:   'review',
  inactive: 'inactive'
};

/**
 * ParseTagType enum
 */
var ParseTagType = {
  HashTag: 0,
  UserTag: 1
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
  type                : {type: String, default: 'user'},
  subtype             : {type: String, default: null},
  scope_modify_date   : {type: Date, default: null},
  accolade_target     : {type: String, default: null}
}, { versionKey: false});

var homeFields = function(creator){
  return {id: 1, creator: 1, text: 1, file_path: 1, likes_count: 1, 
    likes: {$elemMatch: {_id: creator}}, category: 1, external_provider: 1,
    comments_count:1 , create_date: 1, location_latitude: 1, location_longitude: 1,
    hash_tags: 1, status: 1, tags: 1
  };
}

postSchema.statics.canResolve = function(){
  return [
    {creator: {identifier: '_id', model: 'User'}},
    {comments: {identifier: 'creator', model: 'User'}},
    {likes: {identifier: '_id', model: 'User'}},
    {origin_post_id: {identifier: '_id', model: 'Post'}},
    {tags: {identifier: '_id', model: 'User'}}
  ];
};

postSchema.statics.selectFields = function(type){
  if(type === 'short'){
    return ['_id','text','category','create_date','file_path',
            'location_name','location_longitude','location_latitude',
            'creator','likes_count','comments_count','scope',
            'hash_tags','hash_tags_count', 'tags', 'tags_count',
            'scope_modify_date', 'accolade_target', 'external_provider',
            'is_flagged', 'flagged_count', 'subtype'];
  }else{
    return ['_id','text','category','create_date','file_path',
            'location_name','location_longitude','location_latitude',
            'creator','likes_count','comments_count','scope',
            'status','hash_tags','hash_tags_count', 'tags', 'tags_count',
            'is_repost','origin_post_id','modify_date', 'delete_date',
            'scope_modify_date', 'accolade_target', 'external_provider',
            'is_flagged', 'flagged_count', 'subtype'];
  }
};

/**
 * Fetchs Post counts by category with a given date (week/year) & offset
 *
 * @param {String} user_id The post creator id
 * @param {Number} week The week of year
 * @param {Number} year The year
 * @param {Number} offset The numerical week offset
 * @param {Function} cb The callback block/function to be invoked
 */
postSchema.static('fetchCategoryPostCountByWeekAndYear', function(user_id, week, year, offset, cb){
  var start_week, end_week, all_time = false;
  if(!week && !year && !offset) all_time = true;
  if(!all_time){
    start_week = new _moment();
    start_week.weekYear(year);
    start_week.week(week + 1);
    start_week.startOf('week');
    var lastWeek = week + offset;
    var lastYear = year;
    end_week = new _moment();
    end_week.weekYear(year);
    end_week.week(lastWeek);
    end_week.endOf('week');
  }

  //convert user_id into ObjectId if it is passed as a string
  //-- which most likely it will be.
  if(typeof(user_id) === 'string') user_id = _mongoose.Types.ObjectId(user_id);

  var criteria = {
    creator: user_id,
    status: 'active',
  };

  var project = {
    category: 1
  };

  var group = {
    _id: {'category': '$category'},
    count: {$sum :1}
  };

  if(!all_time){
    criteria.create_date = {
      $gt: new Date(start_week.toISOString()),
      $lt: new Date(end_week.toISOString())
    };
    project.week = {$week: "$create_date"};
    project.year = {$year: "$create_date"};
    group._id.week = "$week";
    group._id.year = "$year";
  }

  var aggregate = this.aggregate([
    { $match:   criteria },
    { $project: project },
    { $group:   group },
    { $sort:    {count: -1} }
  ]);

  aggregate.exec(function(err, result){
    console.log(err);
    cb(err, result);
  });
});

/**
 * Fetchs Post Hashtags by Category
 *
 * if now category is passed, it retrieves all categories and hashtags
 *
 * @param {String} user_id The post creator id
 * @param {String} category The post category type
 * @param {Function} cb The callback function/block to be invoked
 */
postSchema.static('fetchHashtagsByCategory', function(user_id, category, cb){
  var has_category = false, criteria, project, group;

  if(!user_id)
    throw new Error('A user_id must be supplied');

  if(category)
    has_category = true;

  //convert user_id into ObjectId if is string
  //(can possible be passed as a ObjectId depending on the source)
  if(typeof(user_id) === 'string')
    user_id = _mongoose.Types.ObjectId(user_id);

  criteria = {
    creator: user_id,
    status: 'active'
  };

  project = {
    category: 1,
    hash_tags: 1
  };

  group = {
    _id: { 
      category: "$category",
      hash_tags: "$hash_tags"
    },
    count: {$sum: 1}
  };

  if(has_category)
    criteria.category = category;

  var aggregate = this.aggregate([
    { $match:   criteria },
    { $unwind:  "$hash_tags" },
    { $project: project },
    { $group:   group }
  ]);

  aggregate.exec(function(err, result){
    cb(err,   result);
  });


});

/**
 * When a luminary is confirmed, this method updates all of there
 * posts subtype to luminary. 
 *
 *    shold really only be implemented when a "regular" user 
 *    accepts a trust from an institution
 *
 * @param {String} user_id The post creator id
 * @param {Function} cb The callback function to be invoked
 */
postSchema.static('updateSubtypeToLuminary', function(user_id, cb){
  this.update(
    {creator: user_id},
    {$set: {subtype: "luminary"}},
    {multi: true},
    function(err, result) {
      cb(err, result);
  });
});

postSchema.methods.parseAndUpdateTags = function(){
  var user_tage = [], hash_tag = [];

  if(this.text){
    user_tag = this.text.match(/(@(\S+))/g);
    hash_tag = this.text.match(/(#(\S+))/g);
    this.tagHandler(ParseTagType.UserTag, user_tag);
    this.tagHandler(ParseTagType.HashTag, hash_tag);
  }
};

postSchema.methods.tagHandler = function(type, parsed_array){
  if(_.isArray(parsed_array)){
    if(parsed_array.length > 0){
      if(type === ParseTagType.HashTag){
        for(var idx in parsed_array){
          //strip # character from tag
          parsed_array[idx] = parsed_array[idx].replace(/#/, "");
        }
        this.hash_tags = parsed_array;
        this.hash_tags_count = this.hash_tags.length;
      }
      if(type === ParseTagType.UserTag){
        for(var i = 0; i < parsed_array.length; i++){
          //strip the @ character from tag
          parsed_array[i] = parsed_array[i].match(/([a-zA-Z0-9]+)/)[0];
          var user_id = parsed_array[i].replace(/@/, "");
          var tag_added = false;

          //ensure parsed identifier has the structure of a hex string
          var checkForHexObjectId = new RegExp("^[0-9a-fA-F]{24}$");
          if(checkForHexObjectId.test(user_id)){
            if(this.tags.length === 0){
              this.tags.push({_id: user_id});
              tag_added = true;

            }else{
              var item = function(i){
                return i._id === user_id;
              };

              if(_.filter(this.tags, item).length === 0){
                this.tags.push({_id: user_id});
                tag_added = true;
              }
            }
          }

          if(tag_added) this.sendTagActivityEvent(user_id);
        }
      }
    }
  }
};

postSchema.methods.sendTagActivityEvent = function(user_id){
  var from_user, to_user, post_id;

  to_user   = user_id.toString();

  if(_.isUndefined(this.creator._id)){
    from_user = this.creator.toString();
  }else{
    from_user = this.creator._id.toString();
  }

  post_id = this._id.toString();

  //register tagged activity event
  _utils.registerActivityEvent(to_user, from_user, 'tag', post_id);
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
    accolade_target:      this.accolade_target,
    likes_count:          this.likes_count,
    comments_count:       this.comments_count,
    hash_tags_count:      this.hash_tags_count,
    hash_tags:            this.hash_tags,
    tags:                 this.tags,
    tags_count:           this.tags_count,
    scope:                this.scope,
    scope_modify_date:    this.scope_modify_date,
    is_flagged:           this.is_flagged,
    flagged_count:        this.flagged_count,
    subtype:              this.subtype
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
  return /Aspirations|Aspiration|Passions|Passion|Experiences|Experience|Achievements|Achievement|Inspirations|Inspiration|Personal|Personals|Accolade|Accolades/i.test(value);
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
  if (this.isModified('scope')){
    if (this.scope == 'public') {
      this.create_date = Date.now();
    }
  }
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


postSchema.statics.fetchHomeFeed = function(uid, criteria, next) {
  var model = this.model('Post');
  model.find(criteria)
  .select(homeFields(uid))
  .sort({create_date: -1})
  .limit(10)
  .exec(function(err, posts){
    if (err) console.log(err);
    model.populate(posts, {path: 'creator', model: 'User',
      select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}}, 
      function(err, posts){
        model.populate(posts, {path: 'tags._id', model: 'User',
         select: {_id: 1, name: 1}}, function(err, posts){ 
          var returnData = flatten(posts, uid);
          next(err, returnData);
         });
      });
    
  });
};

postSchema.statics.likePost = function(pid, uid, next){
  var model = this.model('Post');
  model.findOne({_id: pid}, function(err, post){
    if (post) {
      var exists = false;
      _.each(post.likes, function(l){
        if (String(l._id) == String(uid)) {
          exists = true;
        }
      });
      if (!exists) {
        post.likes.push({_id: uid});
        post.likes_count += 1;
      }
      post.save(function(err, post){
        model.populate(post, {path: 'creator', model: 'User', select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}}, function(err, post){
          model.populate(post, {path: 'tags._id', model: 'User',
            select: {_id: 1, name: 1}}, function(err, post){
              _utils.registerActivityEvent(post.creator._id,
                                           uid,
                                           'like',
                                           post._id);
              next(err, flatten([post], uid)[0]);
            });
        });
      });
    } else {
      next (err, post);
    }
  });

};

postSchema.statics.unlikePost = function(pid, uid, next) {
  var model = this.model('Post');
  model.findOne({_id: pid}, function(err, post){
    if (post) {
      var index = -1;
      _.each(post.likes, function(l, idx){
        if (String(l._id) == String(uid)) {
          index = idx;
        }
      });
      if (index != -1) {
        post.likes.splice(index, 1); 
        post.likes_count -= 1;
      }
      post.save(function(err, post){
         model.populate(post, {path: 'creator', model: 'User', select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}}, function(err, post) {
            model.populate(post, {path: 'tags._id', model: 'User',
              select: {_id: 1, name: 1}}, function(err, post){
            post = post.toObject();
            post.likes = [];
            next(err, flatten([post], uid)[0]);
          });
        });
      });

    } else {
      next(err, post);
    }
  });

};

postSchema.statics.createComment = function(pid, creator, text, next){
  var model = this.model('Post');
  var Comment = this.model('Comment');
  model.findOne({_id: pid})
  .exec(function(err, post){
    if (post) {
      var comment = new Comment({creator: creator, text: text, 
        createDate: Date.now()});
      post.comments.push(comment);
      post.save(function(err, post){
        model.populate(post, {path: 'comments.creator', model: 'User', 
         select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}
        }, function(err, post){
          model.populate(post, {path: 'comments.tags._id', model: 'User',
            select: {_id: 1, name: 1}}, function(err, post){
              resolveTags(post, function(err, users){
                next(err, flattenComments(post.comments, creator, users));
              });
            });
        }); 
      });
    } else {
      next(err, post);
    }
  });
};

postSchema.statics.fetchComments = function(pid, requestor, next) {
  var model = this.model('Post');
  model.findOne({_id: pid})
  .select({
    comments: 1 
   
  })
  .exec(function(err, post){
    if (post) {
      model.populate(post, {path: 'comments.creator', model: 'User',
        select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}
      }, 
        function(err, post){
        model.populate(post, {path: 'comments.tags._id', model: 'User', 
          select: {_id: 1, name: 1}}, function(err, post){
            resolveTags(post, function(err, users){
              next(err, flattenComments(post.comments, requestor, users));
            });
        });
      });
    } else {
      next(err, post);
    }
  });

};

postSchema.statics.fetchCommentLikes = function(pid, cid, next){
  var model = this.model('Post');
  model.findOne({_id: pid})
  .select({comments: 1})
  .exec(function(err, post){
    model.populate(post, {path: 'comments.likes._id', model: 'User', 
      select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}},
      function(err, post){
      if (post) {
        var comment = _.find(post.comments, function(c){
          return String(c._id) == String(cid);
        });
        var likes = [];
        if (comment) {
          likes = _.pluck(comment.likes, '_id'); 
        }
        next(err, likes);
      } else {
        next(err, post);
      }
    });
  });
};

postSchema.statics.likeComment = function(pid, cid, uid, next){
  var model = this.model('Post');
  model.findOne({_id: pid})
  .select({comments: 1})
  .exec(function(err, post) {
    if (post) {
      var liked = false;
      _.each(post.comments, function(c){
        if (String(c._id) == String(cid)) {
          _.each(c.likes, function(l){
            if (String(l._id) == String(uid)) {
              liked = true;
            }
          });
          if (!liked) {
            c.likes.push({_id: uid});
            c.likes_count += 1;
          }
        }
      });
      post.markModified('comments');
      post.save(function(err, post){
        model.populate(post, {path: 'comments.creator', model: 'User',
          select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}},
          function(err, post){
        model.populate(post, {path: 'comments.tags._id', model: 'User', 
          select: {_id: 1, name: 1}}, function(err, post){
            resolveTags(post, function(err, users){
              next(err, flattenComments(post.comments, uid, users));
            });
        });
      });
      });
    } else {
      next(err, post);
    }
  });
};

postSchema.statics.unlikeComment = function(pid, cid, uid, next) {
  var model = this.model('Post');
  model.findOne({_id: pid})
  .select({comments: 1})
  .exec(function(err, post) {
    if (post) {
      var index = -1;
      _.each(post.comments, function(c){
        if (String(c._id) == String(cid)) {
          _.each(c.likes, function(l, idx){
            if (String(l._id) == String(uid)) {
              index = idx;
            }
          });
          if (index != -1) {
            c.likes.splice(index, 1);
            c.likes_count -= 1;
          }
        }
      });
      post.markModified('comments');
      post.save(function(err, post){
         model.populate(post, {path: 'comments.creator', model: 'User',
          select: {_id: 1, name: 1, profile_photo_url: 1, type: 1, subtype: 1}},
          function(err, post){

        model.populate(post, {path: 'comments.tags._id', model: 'User', 
          select: {_id: 1, name: 1}}, function(err, post){
            resolveTags(post, function(err, users){
              next(err, flattenComments(post.comments, uid, users));
            });
        });
        });
      });
    } else {
      next(err, post);
    }
  });
}

var flattenComments = function(comments, uid, users) {
  var returnData = [];
  _.each(comments, function(c) {
    if (typeof c.toObject != 'undefined')
      c = c.toObject();
    c.time_since = time.timeSinceFormatter(c.create_date);
    c.creator_id = c.creator._id;
    c.own_comment = (String(c.creator_id) == String(uid));
    if (c.creator) {
      c.creator_name = c.creator.name;
      c.creator_profile_photo_url = c.creator.profile_photo_url;
      c.creator_type = c.creator.type;
      c.creator_subtype = c.creator.subtype;
    }
    delete c.creator;
    matchUserTags(c, users);
    delete c.tags;
    delete c.hash_tags;
    delete c.hash_tags_count;
    delete c.tags_count;
    c.comment_liked = false;
    _.each(c.likes, function(l){
      if (String(l._id) == String(uid)) {
        c.comment_liked = true;
      }
    });
    delete c.likes;
    returnData.push(c);
  });
  return returnData;
};

var flatten = function(posts, uid ){
  var returnData = [];
  _.each(posts, function(p) {
    if (typeof p.toObject != 'undefined')
      p = p.toObject();
    p.time_since = time.timeSinceFormatter(p.create_date);
    p.creator_id = p.creator._id;
    p.own_post =  (String(p.creator_id) == String(uid)); 
    p.creator_name = p.creator.name;
    p.creator_profile_photo_url = p.creator.profile_photo_url;
    p.creator_type = p.creator.type;
    p.creator_subtype = p.creator.subtype;
    delete p.creator;
    p.is_liked = p.likes.length > 0;
    delete p.likes;
    hashTags = [];
    _.each(p.hash_tags, function(h){
      h = '#' + h.toLowerCase();
      hashTags.push(h);
    }); 
    p.hash_tags = hashTags.join(' ');
    matchTags(p);
    returnData.push(p);
  });
  return returnData;
};

var resolveTags = function(obj, next){
  var User = _mongoose.model('User');
  User.resolvePostTags(obj, function(err, users){
    next(err, users);  
  });
}

var matchUserTags = function(obj, users) {
  var at = obj.text;
  if (obj.text) {
    var match = String(obj.text).match(/@\S{24}/g);
    if (match && match.length > 0) {
      _.each(match, function(m){
        var uid = m.substr(1);
        var mu = _.find(users, function(u){
          return (String(u._id) == String(uid));
        });
        if (mu) {
          at = at.replace(m, '@(' + mu.name + '|' + mu._id + ')');
        }
        obj.text = at;
      });
    }
  }
};

var matchTags = function(obj) {
  var at = obj.text;
  if (obj.text) {
    var match = String(obj.text).match(/@\S{24}/g);
    if (match && match.length > 0) {
      _.each(match, function(tag, idx, list) {
        var uid = tag.substr(1);
        var mu = _.find(obj.tags, function(o){
          return (String(o._id._id) == String(uid));
        });
        if (mu) {
          at = at.replace(tag, '@(' + mu._id.name + '|' + mu._id._id + ')');
        }
        obj.text = at;
      });
    }
  }
  delete obj.tags;
}


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

postSchema.statics.readHashTags = function(filter, limit, skip, format, next) {
  var model = this.model('Post');
  var expression = new RegExp(filter, 'i'); 
  var match = {
    status: 'active'
  };
  var secondMatch = {};
  if (filter) {
    match.hash_tags = expression;
    secondMatch = {_id: expression};
  }
  model.aggregate([
    {$match: match},
    {$project: {hash_tags: 1}},
    {$unwind: '$hash_tags'},
    {$group: {_id: '$hash_tags', count: { $sum: 1}}},
    {$match: secondMatch},
    {$sort: {count: -1}},
    {$skip: skip},
    {$limit: limit}
  ])
  .exec(function(err, tags) {
    var results;
    if (format == 'tags_only') {
      results = _.pluck(tags, '_id');
    } else if (format == 'counts') {
      _.each(tags, function(tag){
        tag.tag = tag._id;
        delete tag._id;
      });
      results = tags;
    }
    next(err, results);
  });
};

exports.Post    = _mongoose.model('Post', postSchema);
exports.Comment = _mongoose.model('Comment', commentSchema);
