"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.config = void 0;
var dotenv_1 = __importDefault(require("dotenv"));
var path_1 = __importDefault(require("path"));
dotenv_1.default.config({
    path: process.env.NODE_ENV === 'production'
        ? path_1.default.join(__dirname, '..', '..', '.env')
        : process.env.NODE_ENV === 'testing'
            ? path_1.default.join(__dirname, '..', '..', '.env.test')
            : path_1.default.join(__dirname, '..', '..', '.env.development'),
});
exports.config = {
    application: {
        port: process.env.PORT,
        name: process.env.NAME,
    },
    db: {
        database: process.env.DB_NAME,
        user: process.env.DB_USER,
        password: process.env.DB_PASSWORD,
        host: process.env.DB_HOST,
        port: process.env.DB_PORT,
    },
    jwt: {
        at: process.env.ACCESS_TOKEN_SECRET,
        rt: process.env.REFRESH_TOKEN_SECRET,
    },
};
console.log(exports.config);
