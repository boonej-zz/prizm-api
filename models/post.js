/**
 * Post Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose')
  , _serial     = require('serializer')
  , _crypt      = require('crypto')
  , _utils      = require(process.env.PRISM_HOME + 'utils')
  , User 				= require(process.env.PRISM_HOME + 'models/user');

/**
 * Posts Model Schema
 * @type {Mongoose.Schema}
 */
var postSchema = new _mongoose.Schema({
	text 						: {type: String, default: null},
	type 						: {type: String, required:true},
	create_date			: {type: Date, default:null, index: true},
	modify_date			: {type: Date, default: Date.now()},
	delete_date			: {type: Date, default: null},
	scope 					: {type: String, default: 'public'},
	location 				: {name: String, longitude: Number, latitude: Number},
	creator 				: {id: String, name: String},
	target_id				: {type: String, required: true},
	status 					: {type: String, default: 'active'},
	file_path 			: {type: String, default: ''},
	likes_count 		: Number,
 	comments_count 	: Number,
 	comments 				: [],
 	likes 					: []
}, { versionKey: false});


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
		this.create_date = Date.now();
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

exports.Post = _mongoose.model('Post', postSchema);