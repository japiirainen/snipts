"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ValidationFailed = exports.InvalidRequest = exports.processError = void 0;
const ts_custom_error_1 = require("ts-custom-error");
const uuid_1 = require("uuid");
const logger_1 = require("./logger");
const processError = (res) => (err) => {
    if (!err.code && !err.status) {
        err = new UnexpectedError(err.message);
    }
    if (err.log) {
        logger_1.logger.error(err);
    }
    res.status(err.status).json({ code: err.code });
};
exports.processError = processError;
// General errors
class UnexpectedError extends ts_custom_error_1.CustomError {
    constructor() {
        super(...arguments);
        this.status = 500;
        this.code = uuid_1.v4();
        this.log = true;
    }
}
class InvalidRequest extends ts_custom_error_1.CustomError {
    constructor() {
        super(...arguments);
        this.status = 500;
        this.code = 'InvalidRequest';
        this.log = false;
    }
}
exports.InvalidRequest = InvalidRequest;
class ValidationFailed extends ts_custom_error_1.CustomError {
    constructor() {
        super(...arguments);
        this.status = 500;
        this.code = 'ValidationFailed';
        this.log = false;
    }
}
exports.ValidationFailed = ValidationFailed;
