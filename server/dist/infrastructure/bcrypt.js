"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.hashPassword = exports.comparePasswords = exports.BcryptError = void 0;
const bcrypt_1 = require("bcrypt");
const TE = __importStar(require("fp-ts/TaskEither"));
const ts_custom_error_1 = require("ts-custom-error");
const uuid_1 = require("uuid");
class BcryptError extends ts_custom_error_1.CustomError {
    constructor() {
        super(...arguments);
        this.status = 500;
        this.code = uuid_1.v4();
        this.log = true;
    }
}
exports.BcryptError = BcryptError;
const comparePasswords = (hashedPassword, attempt) => TE.tryCatch(() => bcrypt_1.compare(attempt, hashedPassword), _ => new BcryptError('error while comparing passwords'));
exports.comparePasswords = comparePasswords;
const hashPassword = (password) => TE.tryCatch(() => bcrypt_1.hash(password, 10), _ => new BcryptError('error while hashing password'));
exports.hashPassword = hashPassword;
