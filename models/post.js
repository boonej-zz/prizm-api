/**
 * Post Model
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose')
  , _serial     = require('serializer')
  , _crypt      = require('crypto')
  , _utils      = require(process.env.PRISM_HOME + 'utils');

var postSchema = new _mongoose.Schema({
	text 						: {type: String, default: null},
	type 						: {type: String, required:true},
	create_date			: {type: Date, default: Date.now()},
	modify_date			: {type: Date, default: Date.now()},
	delete_date			: {type: Date, default: null},
	scope 					: {type: String, default: 'public'},
	location 				: {name: String, longitude: Number, latitude: Number},
	creator 				: {id: String, name: String},
	status 					: {type: String, default: 'active'},
	file_path 			: {type: String, default: ''},
	likes_count 		: Number,
 	comments_count 	: Number,
 	comments 				: [],
 	likes 					: []
}, { versionKey: false});

exports.Post = _mongoose.model('Post', postSchema);