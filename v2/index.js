var express = require('express');
var app = express();
var organizations = require('./organizations');
var users = require('./users');
var interests = require('./interests');
var posts = require('./posts');
var hashtags = require('./hashtags');
var insights = require('./insights');

/**
 * @apiDefine Error
 * @apiError (Error 400) invalid_request The request was malformed or 
 *  otherwise unusable.
 * @apiErrorExample Bad Request:
 *  HTTP/1.1 400 Bad Request
 *  {
 *    "error": "invalid_request",
 *    "description": "You must specify a user id."
 *  }
 * @apiError (Error 500) server_error There was a server side processing
 *  error during the request.
 * @apiErrorExample Server Error:
 *  HTTP/1.1 500 Internal Server Error
 *  {
 *    "error": "server_error",
 *    "description": "There was a problem processing your request."
 *  }
**/

/**
 * @apiDefine UserShortSuccess
 * @apiSuccess {String} _id Unique ID of user
 * @apiSuccess {String} subtype Subtype of user
 * @apiSuccess {String} type Type of user
 * @apiSuccess {String} profile_photo_url Path to user avatar
 * @apiSuccess {String} user Friendly user name
 **/

/**
 * @apiDefine UserMinimalArray
 * @apiSuccess {User[]} body Array of users
 * @apiSuccess {String} _id Unique ID of user
 * @apiSuccess {String} name Name of user
 * @apiSuccess {String} profile_photo_url Path to user avatar
 * @apiSuccessExample Success Response:
 *  HTTP/1.1 200 OK
 *    [
 *      {
 *        "_id": "K3mOksajkskqR3G3AANbbmks",
 *        "name": "Bill Doe",
 *        "profile_photo_url": "https://test.com/somephoto.jpg"
 *      },
 *      {
 *        "_id": "mmKSKLd88AkdmlkOJFLemels",
 *        "name": "Jane Doe",
 *        "profile_photo_url": "https://test.com/someotherphoto.jpg"
 *      }
 *    ]
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
 * @apiSuccessExample Success-Response:
 *  HTTP/1.1 200 OK
 *  {
 *    "_id": "a8k2hh3h4j5j2k3l499diosa",
 *    "first_name": "John",
 *    "last_name": "Doe",
 *    "name": "John Doe",
 *    "email": "john@doe.com",
 *    "contact_first": null,
 *    "contact_last": null,
 *    "contact_email": null,
 *    "create_date": "2015-06-17T16:18:09.178Z",
 *    "program_code": null,
 *    "active": true,
 *    "type": "user",
 *    "subtype": null,
 *    "enrollment": null,
 *    "profile_photo_url": "https://example.site/image.jpg",
 *    "cover_photo_url": "https://example.site/cover_photo_image.jpg",
 *    "mascot": null,
 *    "zip_postal": "88222",
 *    "state": "TX",
 *    "city": "Some City",
 *    "birthday": "09-44-1199",
 *    "gender": "male",
 *    "phone_number": "222-222-2222",
 *    "religion": null,
 *    "ethnicity": null,
 *    "website": "https://example.site",
 *    "info": "I like to make cakes.",
 *    "primary_organization": null,
 *    "theme": null,
 *    "role": null,
 *    "interest_count": 6,
 *    "allMuted": false
 *  } 
 **/

app.use('/organizations/', organizations); 
app.use('/users/', users);
app.use('/interests/', interests);
app.use('/posts/', posts);
app.use('/hashtags/', hashtags);
app.use('/insights/', insights);
app.get('/', function(req, res){
  res.send('Welcome to the V2 API');
});

module.exports = app;
