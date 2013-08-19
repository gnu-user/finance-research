/*
 * Script to create all the relation schemas for the vignette website
 */


/*
 * Contains the matching results for each banned symbol.
 */
CREATE TABLE matching
(
    match_id    INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
    symbol      VARCHAR(8) NOT NULL,
    matched     VARCHAR(8) NOT NULL,
    frequency   INTEGER UNSIGNED NOT NULL,
    sum_price   DOUBLE UNSIGNED NOT NULL,
    sum_vol     DOUBLE UNSIGNED NOT NULL,
    sum_cap     DOUBLE UNSIGNED NOT NULL,
    sum_mean    DOUBLE UNSIGNED NOT NULL,
    add_date    DATE NOT NULL,
    exp_date    DATE NOT NULL,
    PRIMARY KEY(match_id)
);


/*
 * Contains the aggregated market data for each symbol each day
 */
CREATE TABLE market_data
(
    time          DATE NOT NULL,
    symbol        VARCHAR(8) NOT NULL,
    match_id      INTEGER UNSIGNED DEFAULT NULL,
    quartile      CHAR(2) NOT NULL,
    transactions  INTEGER UNSIGNED NOT NULL,

    sum_shares    INTEGER UNSIGNED NOT NULL,
    min_shares    INTEGER UNSIGNED NOT NULL,
    mean_shares   DOUBLE UNSIGNED NOT NULL,
    med_shares    DOUBLE UNSIGNED NOT NULL,
    max_shares    INTEGER UNSIGNED NOT NULL,

    min_price     DOUBLE UNSIGNED NOT NULL,
    mean_price    DOUBLE UNSIGNED NOT NULL,
    med_price     DOUBLE UNSIGNED NOT NULL,
    max_price     DOUBLE UNSIGNED NOT NULL,

    min_vol       DOUBLE UNSIGNED NOT NULL,
    mean_vol      DOUBLE UNSIGNED NOT NULL,
    med_vol       DOUBLE UNSIGNED NOT NULL,
    max_vol       DOUBLE UNSIGNED NOT NULL,

    total_vol     DOUBLE UNSIGNED NOT NULL,
    market_cap    DOUBLE UNSIGNED NOT NULL,

    FOREIGN KEY(match_id) REFERENCES matching(match_id)
        ON DELETE SET NULL ON UPDATE CASCADE
);
