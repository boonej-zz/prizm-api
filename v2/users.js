var express = require('express');
var app = express();
var mongoose = require('mongoose');
var ObjectId = mongoose.Types.ObjectId;
var config = require('config');
var gateway = require('../gateway');
var _ = require('underscore');
var moment = require('moment');

var User = mongoose.model('User');
var Interest = mongoose.model('Interest');
var Post = mongoose.model('Post');
var Activity = mongoose.model('Activity');
var Trust = mongoose.model('Trust');
var Error = require('../lib/error');

/**
 * @apiDefine UserShortSuccess
 * @apiSuccess {String} _id Unique ID of user
 * @apiSuccess {String} subtype Subtype of user
 * @apiSuccess {String} type Type of user
 * @apiSuccess {String} profile_photo_url Path to user avatar
 * @apiSuccess {String} user Friendly user name
 **/

/**
 * @apiDefine UserMinimal
 * @apiSuccess {String} _id Unique ID of user
 * @apiSuccess {String} name Name of user
 * @apiSuccess {String} profile_photo_url Path to user avatar
 **/

/**
 * @apiDefine UserCoreSuccess
 * @apiSuccess {String} _id Unique ID of user
 * @apiSuccess {String} first_name First name of user
 * @apiSuccess {String} last_name Last name of user
 * @apiSuccess {String} name Full name of user
 * @apiSuccess {String} email Email address of user
 * @apiSuccess {String} contact_first Organization primary contact first name
 * @apiSuccess {String} contact_last Organization primary contact last name
 * @apiSuccess {String} contact_email Organization primary contact email
 * @apiSuccess {Date} create_date Date user account inititally created
 * @apiSuccess {String} program_code Current program code assigned to the user
 * @apiSuccess {Boolean} active True if user is an active account
 * @apiSuccess {String} type Type for user 
 * @apiSuccess {String} subtype Subtype for user
 * @apiSuccess {Number} enrollment User defined number of members
 * @apiSuccess {String} profile_photo_url Path to user's avatar
 * @apiSuccess {String} cover_photo_url Path to user's cover photo
 * @apiSuccess {String} mascot Mascot for institution
 * @apiSuccess {String} zip_postal Zip code for user
 * @apiSuccess {String} state Geographical state for user
 * @apiSuccess {String} city Geographical city for user
 * @apiSuccess {String} birthday Birthday for user (mm-dd-yyyy)
 * @apiSuccess {String} gender Gender of user
 * @apiSuccess {String} phone_number Phone number for user
 * @apiSuccess {String} religion Religion for user
 * @apiSuccess {String} ethnicity Ethnicity for user
 * @apiSuccess {String} website Website for user
 * @apiSuccess {String} info Biographical info for user
 * @apiSuccess {String} primary_organization Default organization for user
 * @apiSuccess {String} theme Current user theme
 * @apiSuccess {String} role Role for user in primary organization
 * @apiSuccess {Number} interest_count Total number of interests for user
 * @apiSuccess {Boolean} allMuted True if user has muted the primary organization.
 **/

app.get('/', gateway, function(req, res){
  var searchText = req.query.search || false;
  var organization = req.query.organization || false;
  var group = req.query.group || false;
  var limit = req.params.limit || false;
  var params = {};
  if (searchText) {
    var r = new RegExp(searchText, 'i');
    params.name = r;
  }
  if (organization) {
    var orgStatus = {organization: ObjectId(organization), status: 'active'};
    if (group) {
      orgStatus.groups = ObjectId(group);
    }
    params.org_status = {$elemMatch: orgStatus};
  }
  console.log(params);
  User.findBasic(params, limit, function(err, users){
    if (err) {
      res.status(500).json(err);
    } else {
      res.status(200).json(users);
    }
  });
});

/**
 * @api {get} /users/:uid Get User
 * @apiName FetchUserCore
 * @apiVersion 2.0.0
 * @apiParam {String} uid Unique ID for user
 * @apiGroup Users
 * @apiUse UserCoreSuccess
 * @apiUse Error
 **/
app.get('/:uid',  function(req, res){
  var uid = req.params.uid;
  if (uid) {
    uid = uid.replace(" ", "");
    User.findOneCore(uid, function(err, user){
      if (err) console.log(err);
      if (user) {
        res.status(200).json(user);
      } else {
        Error.serverError(res); 
      }
    });
  } else {
    Error.invalidRequest(res, 'You must provide a user id.');
  }
});

// Register Device
/**
 * @api {put} /users/:uid/devices Register Device
 * @apiName RegisterUserDevice
 * @apiVersion 2.0.0
 * @apiGroup Users
 * @apiParam {String} uid Unique id for user
 * @apiParam (Body) {String} device Unique id for device
 * @apiUse UserCoreSuccess 
 * @apiUse Error
 **/
app.put('/:uid/devices', gateway, function(req, res){
  var uid = req.params.uid;
  var device = req.body.device;
  User.registerDevice(uid, device, function(err, user){
    if (err){
      res.status(500).json(err);
    } else {
      res.status(200).json(user);
    }
  });
});

// Update User
/**
 * @api {put} /users/:uid Update User
 * @apiName UpdateUser
 * @apiGroup Users
 * @apiVersion 2.0.0
 * @apiParam {String} uid Unique id for user
 * @apiParam (Body) {String} first_name First name for user
 * @apiParam (Body) {String} last_name Last name for user
 * @apiParam (Body) {String} email Email for user
 * @apiParam (Body) {String} gender Gender for user
 * @apiUse UserCoreSuccess
 * @apiUse Error
 **/
app.put('/:uid', gateway, function(req, res) {
  var uid = req.params.uid;
  var post = req.body;
  if (post.date_founded) {
    post.date_founded = moment(post.date_founded).toDate() || null;
  } 
  User.findOneAndUpdate({_id: uid}, post, function(err, user){
    if (err) {
      console.log(err);
      res.status(500).json(err);
    } else {
      console.log('user updated');
      User.findOneCore(uid, function(err, user){
        if (err) {
          res.status(500).json(err);
          console.log(err);
        } else res.status(200).json(user);
      });
    }
  });
});

// INTERESTS
/**
 * @api {get} /users/:uid/interests Get User Interests
 * @apiName GetUserInterests
 * @apiVersion 2.0.0
 * @apiGroup Users
 * @apiParam {String} uid Unique id for user
 * @apiSuccess {String} _id Unique id for interest
 * @apiSuccess {Boolean} is_subinterest True if interest is a child interest
 * @apiSuccess {Array} subinterests Array of subinterests
 * @apiSuccess {String} text Name of the interest
 * @apiSuccess {Date} create_date Date interest added 
 * @apiUse Error
 **/
app.get('/:uid/interests', gateway, function(req, res){
  var uid = req.params.uid;
  User.findOne({_id: uid}, {_id: 1, interests: 1}, function(err, user){
    if (err) res.status(500).send(err);
    else {
      Interest.find({})
      .exec(function(err, interests){
        var result = [];
        if (user && _.isArray(user.interests)) {
          _.each(interests, function(i){
            i = i.toObject();
            var selected = false;
            _.each(user.interests, function(ui){
              var comp = ui._id?String(ui._id):String(ui);
              if (String(i._id) == String(comp)) {
                selected = true;
              }
            });
            i.selected = selected;
            result.push(i);
          });
        } else {
          _.each(interests, function(i){
            i = i.toObject();
            i.selected = false;
            result.push(i);
          });
        }
        res.status(200).json(result);
      });
    }
  });
});

/**
 * @api {put} /users/:uid/interests Update User Interests
 * @apiName UpdateUserInterests
 * @apiVersion 2.0.0
 * @apiGroup Users
 * @apiParam {String} uid Unique id for user
 * @apiParam Body {String[]} interests A list of unique interest ids
 * @apiUse Error
 **/
app.put('/:uid/interests', gateway, function(req, res){
  var uid = req.params.uid;
  var interests = JSON.parse(req.body.interests);
  var newArray = [];
  if (!_.isArray(interests)) interests = [interests];
  _.each(interests, function(i){
    newArray.push(ObjectId(i.uniqueId));
  });
  User.findOne({_id: uid}, function(err, user){
    user.interests = newArray;
    user.save(function(err, user){
      if (err) res.status(500).json(err);
      else res.status(200).json(user);
    });
  });
});

/** Home Feed **/
/**
 * @api {get} /users/:uid/home Get User Home Feed
 * @apiName GetUserHomeFeed
 * @apiVersion 2.0.0
 * @apiGroup Users
 * @apiParam {String} uid Unique id for user
 * @apiParam (Query) {String} [last] Date string for last post
 * @apiSuccess {Object[]} posts List of home feed posts
 * @apiUse Error
 **/
app.get('/:uid/home', gateway, function(req, res) {
  var uid = req.params.uid;
  var last = req.query.last;
  User.fetchHomeFeedCriteria(uid, last, function(err, criteria){
    if (err) res.status(400).json(err);
    else {
      Post.fetchHomeFeed(uid, criteria, function(err, posts){
        if (err) res.status(500).json(err);
        else res.status(200).json(posts);
      });
    }
  });
}); 

/** Activity **/
/**
 * @api {get} /users/:uid/activities Get Activity Feed
 * @apiName GetActivityFeed
 * @apiVersion 2.0.0
 * @apiGroup Users
 * @apiParam {String} uid Unique id for user
 * @apiParam {String} [last] Date of last activity retrieved
 * @apiParam {String="count"} [filter] Sets filter for only activity counts
 * @apiUse Error
**/
app.get('/:uid/activities', gateway, function(req, res) {
  var uid = req.params.uid;
  var last = req.query.last;
  var filter = req.query.filter;
  if (filter == 'counts') {
    var result = {};
    Activity.count({to: uid, has_been_viewed: false}, function(err, c){
      result.activities = c;
      Trust.count({to: uid, status: 'pending'}, function(err, pc){
        result.trusts = pc;
        res.json(result);
      });
    });
  } else {
    Activity.fetchActivitiesForUser(uid, last, function(err, activities){
      if (err)res.json(err);
      else res.json(activities);
    });
  }
});

/** Trusts **/
/**
 * @api {get} /users/:uid/trusts Get Trusts
 * @apiName GetTrusts
 * @apiVersion 2.0.0
 * @apiGroup Users
 * @apiParam {String} uid Unique id for user
 * @apiParam {String} [last] Name of last user pulled
 * @apiParam {String="activity"} [filter] Sets filter for trusts
 * @apiUse UserShortSuccess
 * @apiUse Error
 **/
app.get('/:uid/trusts', function(req, res) {
  var uid = req.params.uid;
  var last = req.query.last;
  var filter = req.query.filter;
  if (filter == 'activity') {
    Trust.fetchTrustActivityForUser(uid, last, function(err, trusts){
      if (err) res.status(400).json(err);
      else res.status(200).json(trusts);
    });
  }
});

/** Tags **/
/**
 * @api {get} /users/:uid/tags Get Available Tags
 * @apiName GetAvailableTags
 * @apiVersion 2.0.0
 * @apiGroup Users
 * @apiParam {String} uid Unique ID for user
 * @apiParam (Query) {String} tag Tag to search for 
 * @apiUse UserMinimal
 * @apiUse Error
 **/
app.get('/:uid/tags', gateway, function(req, res) {
  var uid = req.params.uid;
  var text = req.query.tag;
  if (uid) {
    User.fetchAvailableTags(uid, text, function(err, users){
      if (err) Error.serverError(res);
      else res.status(200).json(users);
    });
  } else {
    Error.invalidRequest(res, 'You must provide a user id. ');
  }
});

module.exports = app;
