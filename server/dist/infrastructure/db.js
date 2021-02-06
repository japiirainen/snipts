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
exports.withConn = exports.createDbPool = exports.DBError = void 0;
const TE = __importStar(require("fp-ts/TaskEither"));
const pg_1 = require("pg");
const postgres_migrations_1 = require("postgres-migrations");
const ts_custom_error_1 = require("ts-custom-error");
const uuid_1 = require("uuid");
const config_1 = require("./config");
const logger_1 = require("./logger");
class DBError extends ts_custom_error_1.CustomError {
    constructor() {
        super(...arguments);
        this.status = 500;
        this.code = uuid_1.v4();
        this.log = true;
    }
}
exports.DBError = DBError;
const createDbPool = async () => {
    const pool = new pg_1.Pool({
        database: config_1.config.db.database,
        user: config_1.config.db.user,
        password: config_1.config.db.password,
        host: config_1.config.db.host,
        port: Number(config_1.config.db.port),
    });
    try {
        const client = await pool.connect();
        try {
            await postgres_migrations_1.migrate({ client }, 'sql');
        }
        finally {
            await client.release();
        }
    }
    catch (e) {
        logger_1.logger.error('Failed to connect to db');
        return null;
    }
    return pool;
};
exports.createDbPool = createDbPool;
const withConn = (pool, f) => TE.tryCatch(async () => {
    const client = await pool.connect();
    try {
        return await f(client);
    }
    catch (e) {
        throw new DBError(e.message);
    }
    finally {
        await client.release();
    }
}, () => new DBError('db error'));
exports.withConn = withConn;
