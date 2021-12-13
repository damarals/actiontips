


CREATE TABLE stat(tipper_id VARCHAR, league VARCHAR, period VARCHAR, win VARCHAR, loss VARCHAR, count VARCHAR, PRIMARY KEY(tipper_id, league, period));
CREATE TABLE tipper(tipper_id VARCHAR, "name" VARCHAR, is_expert VARCHAR, is_author VARCHAR, is_verified VARCHAR, num_followers VARCHAR, PRIMARY KEY(tipper_id));
CREATE TABLE pick(pick_id VARCHAR, tipper_id VARCHAR, game_id VARCHAR, created_at VARCHAR, updated_at VARCHAR, league VARCHAR, tip_play VARCHAR, tip_type VARCHAR, PRIMARY KEY(pick_id));



