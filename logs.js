var _winston = require('winston');

var test_mode_exception_handlers = false;
var exception_handlers = [
		new _winston.transports.File({
			filename: 'logs/prism_errors.log',
			json: true
		}),
		new _winston.transports.Console({
			json: true,
			colorize: true,
			prettyPrint: true
		})
	];

var logger = new (_winston.Logger)({
	transports: [
		// new _winston.transports.Console({ filename: 'logs/prism.log', level: 'debug'}),
		new _winston.transports.File({
			filename: 'logs/prism.log',
			timestamp: true,
			colorize:true
		}),
	],
	exceptionHandlers: (process.env.NODE_ENV !== 'test') ? exception_handlers : test_mode_exception_handlers
});

module.exports = logger;
