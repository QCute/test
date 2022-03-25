#!/bin/sh

sock=/tmp/mysql.sock
user=sdzmmo
pass=sdzmmo123456
src_db=hf_s2
dst_db=hf_s1
src_server_id=2
dst_server_id=1
RoleOffset=""
GoodsOffset=""
GuildOffset=""
PetOffset=""

##################  pre ########################
#当前版本数据库一共有595张表, 2014-1-6
CURRENT_TABLE_NUM=595

SRC_TABLE_NUM=`mysql -u"${user}" -p"${pass}" "${src_db}" -S "${sock}" -e "show tables" |grep -v "Table" |wc -l`
DST_TABLE_NUM=`mysql -u"${user}" -p"${pass}" "${dst_db}" -S "${sock}" -e "show tables" |grep -v "Table" |wc -l`

if [ "${CURRENT_TABLE_NUM}" != "${SRC_TABLE_NUM}" ] || [ "${CURRENT_TABLE_NUM}" != "${DST_TABLE_NUM}" ] ; then
	echo "需求表数：${CURRENT_TABLE_NUM} "
	echo "本地库表数 ${src_db}：${SRC_TABLE_NUM}"
	echo "远程库表数 ${dst_db}: ${DST_TABLE_NUM}"
	echo "合服表数不对，ERR"
  exit 1;
fi
################### end pre ####################


# 将来可能出现的修改：
# 1. 增删表
# 2. 主键改变
# 3. 有表增加了人物、帮会、堂的名称记录字段

# 先修改源数据库，再整个写入目标数据库
DBTables=(
    "appoint_record"
    "baike"
    "bless_card"
	"box_bag"
	"box_player_counter"
	"buff"
    "change_figure"
	"charge"
    "charge_inner"
    "chengjiu"
    "chengjiu_award"
	"collector"
    "combat_power_rank"
    "contact_list"
    "continuous_login_gift"
    "country_treasure_record"
    "couple"
    "couple_home"
    "couple_insure"
    "couple_ring"
    "couple_ring_limit"
    "couple_sign"
    "daily_flower_rank"
    "daily_jyzh"
    "doubt_account"
    "doubt_studio_account"
    "dps"
    "dungeon_entrust"
    "dungeon_entrust_bag"
    "equip_attribute_wash"
    "equip_cl"
    "equip_darkgold_bless"
    "equip_darkgold_wash"
	"equip_rank"
    "equip_skill"
    "equip_soul_forge"
    "equip_top_suit"
    "ever_name"
    "fashion_figure"
	"fcm"
    "fcwr_leave_msg"
    "fcwr_user"
	"feedback"
    "fly_egg"
    "fortune"
    "fortune_refresh_log"
    "friend_tree"
    "fxgz"
	"gift_list"
	"goods"
    "goods_darkgold"
	"goods_high"
	"goods_low"
    "goods_sign"
	"guild"
    "guildbossmgr"
	"guild_apply"
    "guild_award"
    "guild_award_alloc"
    "guild_banquet"
    "guild_chengjiu"
    "guild_chengjiu_reward"
    "guild_dungeon"
    "guild_member_award"
	"guild_invite"
	"guild_member"
    "guild_member_skill"
    "guild_skill"
    "hanger"
    "hero_back_card"
    "kfz_3v3_award"
    "kfz_arena_award"
    "kfz_league_audience"
    "kfz_boss_reward"
    "kfz_friend_gift"
    "kfz_alliance_log"
    "kfz_alliance_result"
    "kfz_alliance_result1"
    "kfz_league_result"
    "kfz_league_result1"
    "kfz_league_bet"
    "kfz_quiz_result"
    "kfz_quiz_result1"
    "kfz_quiz_award"
    "kfz_star_result"
    "kfz_team"
    "kfz_territory_reward"
    "log_activity"
    "log_anqi_mastery"
    "log_anqi_skill"
    "log_anqi_upgrade"
    "log_attribute_wash"
	"log_backout"
    "log_bless_box"
	"log_box"
    "log_chengjiu"
    "log_comeback"
	"log_compose"
	"log_consume_coin"
	"log_consume_gold"
    "log_consume_point"
    "log_couple_ring_open"
	"log_daily"
    "log_darkgold_backout"
    "log_darkgold_bless"
    "log_darkgold_inlay"
    "log_darkgold_purity"
    "log_darkgold_upgrade"
    "log_darkgold_wash"
    "log_dungeon_entrust"
    "log_equip_cl"
	"log_equip_compose"
    "log_equip_forge"
    "log_equip_refine"
    "log_equip_skill"
	"log_exchange"
    "log_expose"
    "log_fashion_change"
    "log_fashion_ticket"
    "log_fly_egg_strln"
	"log_forge"
    "log_fortune"
    "log_friend_delete"
    "log_fxgz"
    "log_gift"
    "log_gm_mail"
	"log_goods"
    "log_goods_merge"
    "log_goods_reclaim"
    "log_goods_sell"
    "log_goods_sign"
    "log_goods_smelt"
    "log_goods_use"
    "log_group_buying"
    "log_guild_award"
    "log_guild_banquet_begin"
    "log_guild_banquet_book"
    "log_guild_dial"
	"log_guild_goods"
    "log_guild_mall_exchange"
    "log_guild_skill_active"
    "log_guild_skill_point"
    "log_hanger"
    "log_happy_bag"
	"log_hole"
	"log_inlay"
    "log_jy"
    "log_kfz_terr_setting"
    "log_kfz_terr_tender"
    "log_kfz_terr_reward"
    "log_login"
    "log_lucky_box"
	"log_mail_info"
    "log_material"
    "log_medal_soul"
    "log_meridian"
    "log_mount_active"
    "log_mount_equip"
    "log_mount_equip5x_back"
    "log_mount_equip5x_wash"
    "log_mount_familiar"
    "log_mount_feed"
    "log_mount_return"
    "log_mount_skill"
    "log_mount_stren"
    "log_mount_szbf"
    "log_mount_throw"
    "log_mount_upgrade"
	"log_online_time"
    "log_ore_drop"
    "log_pay"
    "log_personal_dial"
    "log_pet"
    "log_pet_aircraft_active"
    "log_pet_equip_active"
    "log_pet_equip_craft"
    "log_pet_equip_exchange"
    "log_pet_equip_goods"
    "log_pet_equip_reclaim"
    "log_pet_equip_score"
    "log_phase_dungeon"
    "log_prefix_upgrade"
	"log_produce_coin"
	"log_produce_gold"
    "log_rank_gift"
    "log_rank_top_hero_gift"
    "log_rebate"
    "log_rename"
    "log_resolve"
    "log_secret_shop"
	"log_sell"
    "log_soul_forge"
    "log_soul_inpouring"
    "log_stone_compose"
    "log_stone_enchant"
	"log_stren"
    "log_suit_resolve"
    "log_super_stren"
    "log_taobao"
    "log_target_day"
    "log_target_week_task"
    "log_target_week_reward"
    "log_task_cumulate"
    "log_td"
    "log_td1"
	"log_throw"
    "log_top_forging"
    "log_top_suit"
	"log_tower"
	"log_trade"
    "log_treasure_hunt"
    "log_uplv"
    "log_vip_privilege"
    "log_vip_privilege_reward"
    "log_wage"
    "log_warrior"
    "log_warrior_attribute"
	"log_weapon_compose"
    "log_wing_del"
    "log_wing_dungeon_daily_reward"
    "log_wing_dungeon_reward"
    "log_wing_exp"
    "log_wing_fly"
    "log_wing_huanhua"
    "log_wing_skill"
    "log_wing_skill_map"
    "log_wing_stren"
    "log_wing_train"
    "log_wing_practice"
    "log_wtb"
    "lottry_lucker"
    "lottry_tick"
    "lucky_animal"
    "lucky_animal_role"
    "mail_activity"
	"mail_attr"
	"mail_content"
    "mail_queue"
    "marriage_event"
	"master"
	"master_apprentice"
	"meridian"
    "money_tree"
    "money_tree_log"
    "mount"
    "mount_equip"
    "mount_equip5x"
    "mount_skill"
    "mount_szbf"
    "new_ser_gift"
    "offline_msg"
	"pet"
    "pet_aircraft"
    "pet_equip"
    "pet_equip_achieve"
    "pet_equip_box"
    "pet_equip_status"
    "pet_manual"
    "pet_potential"
	"pet_shop"
	"pet_skill"
    "pet_skill_treasurebox"
    "pet_treasurebox"
    "phase_dungeon"
    "phase_dungeon_auto"
	"player_attr"
	"player_high"
	"player_login"
	"player_low"
	"player_state"
    "player_talent"
	"practice"
	"practice_outline"
    "present_flower"
    "rank_history"
    "rank_hurt_of_boss"
    "rank_intraday_charge"
    "rank_intraday_cost"
    "rank_role_popularity"
    "rank_wing_dungeon"
    "relationship"
	"rela_group"
    "role_achieved_name"
    "role_activity"
    "role_anqi"
    "role_bless_box"
    "role_fashion_figure"
    "role_funds"
    "role_funds_new"
    "role_group_buying"
    "role_happy_bag"
    "role_hp_bag"
    "role_olympiad"
	"secondary_password"
    "secret_shop"
    "sell_list"
	"skill"
    "skill2"
    "soul_inpouring"
	"st_pay_user"
	"st_goods"
    "sworn_friends"
    "sworn_member"
    "taobao"
    "target_day"
    "target_week"
    "target_week_task"
	"task_auto"
	"task_bag"
	"task_his"
	"task_log"
    "task_log_clear"
    "treasure_hunt"
    "uplv_act_record"
    "user_offline_status"
    "vip_privilege"
    "warrior"
    "warrior_attribute"
    "wenju_award"
    "wine_outline"
    "wing"
    "wing_dungeon"
    "wing_fashion"
    "wing_practice"
    "wing_skill"
    "wishing_wall"
    "wtb_list"
    "wuju_log"
    "wuju_result"
    "wuju_match_log"
)


function get_id
{
	local query=${1}
	local OriginID=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "$query" | egrep [0-9]+)
	if [ "$OriginID" == "" ]
	then
		echo "0"
	else
		echo "$OriginID"
	fi
}

# 需要修改名称(修改名称将在合服后进行)
function fix_db
{
	#mysql -u$user -p$pass $src_db -S "${sock}" -e "ALTER TABLE player_low CHANGE nickname nickname VARCHAR( 100 ) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL"
	#mysql -u$user -p$pass $dst_db -S "${sock}" -e "ALTER TABLE player_low CHANGE nickname nickname VARCHAR( 100 ) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL"
	# 修改player_login, player_low, guild的唯一索引
	mysql -u$user -p$pass $src_db -S "${sock}" -e "ALTER TABLE player_login DROP INDEX accname , ADD INDEX accname (accname)" 
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "ALTER TABLE player_login DROP INDEX accname , ADD INDEX accname (accname)"
	mysql -u$user -p$pass $src_db -S "${sock}" -e "ALTER TABLE player_low DROP INDEX nickname , ADD INDEX nickname (nickname)" 
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "ALTER TABLE player_low DROP INDEX nickname , ADD INDEX nickname (nickname)"
	mysql -u$user -p$pass $src_db -S "${sock}" -e "ALTER TABLE guild DROP INDEX index_guild_name , ADD INDEX index_guild_name (name)"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "ALTER TABLE guild DROP INDEX index_guild_name , ADD INDEX index_guild_name (name)"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "ALTER TABLE sworn_friends DROP INDEX index_prefix_name , ADD INDEX index_prefix_name (prefix_name)"
	local AutoID=0
	# 添加服务器id
	mysql -u$user -p$pass $src_db -S "${sock}" -e "UPDATE player_login SET server_id=$src_server_id WHERE server_id=0"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE player_login SET server_id=$dst_server_id WHERE server_id=0"
	# 获取目标数据库最大id值，作为源数据库偏移值 ON DUPLICATE KEY UPDATE id=VALUES(id)
	#RoleOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "SELECT MAX(id) FROM player_login" | egrep [0-9]+)
	#GoodsOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "SELECT MAX(id) FROM goods" | egrep [0-9]+)
	#GuildOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "SELECT MAX(id) FROM guild" | egrep [0-9]+)
	#PetOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "SELECT MAX(id) FROM pet" | egrep [0-9]+)
    RoleOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "select AUTO_INCREMENT from information_schema.tables where TABLE_SCHEMA='$dst_db' and  TABLE_NAME='player_login'" | egrep [0-9]+)
    GoodsOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "select AUTO_INCREMENT from information_schema.tables where TABLE_SCHEMA='$dst_db' and  TABLE_NAME='goods'" | egrep [0-9]+)
    GuildOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "select AUTO_INCREMENT from information_schema.tables where TABLE_SCHEMA='$dst_db' and  TABLE_NAME='guild'" | egrep [0-9]+)
    PetOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "select AUTO_INCREMENT from information_schema.tables where TABLE_SCHEMA='$dst_db' and  TABLE_NAME='pet'" | egrep [0-9]+)
    SwornFriendsOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "select AUTO_INCREMENT from information_schema.tables where TABLE_SCHEMA='$dst_db' and  TABLE_NAME='sworn_friends'" | egrep [0-9]+)
    MountOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "select AUTO_INCREMENT from information_schema.tables where TABLE_SCHEMA='$dst_db' and  TABLE_NAME='mount'" | egrep [0-9]+)
    WingOffset=$(mysql -u$user -p$pass $dst_db -S "${sock}" -e "select AUTO_INCREMENT from information_schema.tables where TABLE_SCHEMA='$dst_db' and  TABLE_NAME='wing'" | egrep [0-9]+)
# 根据目标数据库获得的偏移ID值对源数据库进行偏移
	echo "================ start fix origin database ================"
	update_db "UPDATE appoint_record SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM baike")
	update_db "UPDATE baike SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM bless_card")
	update_db "UPDATE bless_card SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE bless_card SET sender=sender+$RoleOffset where sender > 0 ORDER BY sender DESC"
	update_db "UPDATE bless_card SET receiver=receiver+$RoleOffset where receiver > 0 ORDER BY receiver DESC"
	update_db "UPDATE bless_card SET goods_id=goods_id+$GoodsOffset where goods_id > 0 ORDER BY goods_id DESC"
	update_db "UPDATE box_bag SET pid=pid+$RoleOffset ORDER BY pid DESC"
	update_db "UPDATE box_player_counter SET pid=pid+$RoleOffset ORDER BY pid DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM buff")
	update_db "UPDATE buff SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE change_figure SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM charge")
	update_db "UPDATE charge SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM charge_inner")
	update_db "UPDATE charge_inner SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE chengjiu SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE chengjiu_award SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE collector SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "DELETE c FROM continuous_login_gift c LEFT JOIN player_login p ON c.id=p.id WHERE p.id IS NULL"
	update_db "UPDATE continuous_login_gift SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE country_treasure_record SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE combat_power_rank SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE contact_list SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM couple")
	update_db "UPDATE couple SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE couple SET husband=husband+$RoleOffset ORDER BY husband DESC"
	update_db "UPDATE couple SET wife=wife+$RoleOffset ORDER BY wife DESC"
	update_db "UPDATE couple SET guests_a='[]' ORDER BY id DESC"
	update_db "UPDATE couple SET guests_b='[]' ORDER BY id DESC"
	update_db "UPDATE couple_home SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE couple_home SET husband=husband+$RoleOffset ORDER BY husband DESC"
	update_db "UPDATE couple_home SET wife=wife+$RoleOffset ORDER BY wife DESC"
	update_db "UPDATE couple_sign SET couple_id=couple_id+$AutoID ORDER BY couple_id DESC"
	update_db "UPDATE daily_flower_rank SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE daily_jyzh SET pid=pid+$RoleOffset ORDER BY pid DESC"
	update_db "UPDATE doubt_account SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE doubt_studio_account SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE dps SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE dungeon_entrust SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE dungeon_entrust_bag SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE equip_attribute_wash SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset ORDER BY gid DESC"
	update_db "UPDATE equip_cl SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset ORDER BY pid DESC"
	update_db "UPDATE equip_darkgold_bless SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset ORDER BY gid DESC"
	update_db "UPDATE equip_darkgold_wash SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset ORDER BY gid DESC"
	update_db "UPDATE equip_rank SET role_id=role_id+$RoleOffset, id=id+$GoodsOffset ORDER BY role_id DESC"
	update_db "UPDATE equip_skill SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE equip_soul_forge SET gid=gid+$GoodsOffset, role_id=role_id+$RoleOffset ORDER BY gid DESC"
	update_db "UPDATE equip_top_suit SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM ever_name")
	update_db "UPDATE ever_name SET id=id+$AutoID, role_id=role_id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM fashion_figure")
	update_db "UPDATE fashion_figure SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE fashion_figure SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE fcm SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE fcwr_leave_msg SET sid=sid+$RoleOffset, rid=rid+$RoleOffset ORDER BY rid DESC"
	update_db "UPDATE fcwr_user SET id=id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM feedback")
	update_db "UPDATE feedback SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE fly_egg SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE fortune SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM fortune_refresh_log")
	update_db "UPDATE fortune_refresh_log SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE fortune_refresh_log SET refresh_role=refresh_role+$RoleOffset WHERE refresh_role > 0 ORDER BY id DESC"
	update_db "UPDATE friend_tree SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE fxgz SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE gift_list SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE goods SET player_id=player_id+$RoleOffset, id=id+$GoodsOffset ORDER BY id DESC"
	update_db "UPDATE goods_darkgold SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset ORDER BY gid DESC"
	update_db "UPDATE goods_high SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset ORDER BY gid DESC"
	update_db "UPDATE goods_high SET guild_id=guild_id+$GuildOffset where guild_id > 0 ORDER BY guild_id DESC"
	update_db "UPDATE goods_low SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset ORDER BY gid DESC"
	update_db "UPDATE goods_sign SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset ORDER BY gid DESC"
	update_db "UPDATE guild SET id=id+$GuildOffset, initiator_id=initiator_id+$RoleOffset, chief_id=chief_id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE guildbossmgr SET guild_id=guild_id+$GuildOffset ORDER BY guild_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM guild_apply")
	update_db "UPDATE guild_apply SET player_id=player_id+$RoleOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE guild_award SET guild_id=guild_id+$GuildOffset ORDER BY guild_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM guild_award_alloc")
	update_db "UPDATE guild_award_alloc SET player_id=player_id+$RoleOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE guild_banquet SET player_id=player_id+$RoleOffset, guild_id=guild_id+$GuildOffset ORDER BY player_id DESC"
	update_db "UPDATE guild_chengjiu SET guild_id=guild_id+$GuildOffset ORDER BY guild_id DESC"
	update_db "UPDATE guild_chengjiu_reward SET id=id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM guild_dungeon")
	update_db "UPDATE guild_dungeon SET last_set_id=last_set_id+$RoleOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM guild_invite")
	update_db "UPDATE guild_invite SET player_id=player_id+$RoleOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE guild_member SET id=id+$RoleOffset, guild_id=guild_id+$GuildOffset ORDER BY id DESC"
	update_db "UPDATE guild_member_award SET pid=pid+$RoleOffset, guild_id=guild_id+$GuildOffset ORDER BY pid DESC"
	update_db "UPDATE guild_member_skill SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE guild_skill SET guild_id=guild_id+$GuildOffset ORDER BY guild_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM hanger")
	update_db "UPDATE hanger SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM hero_back_card")
	update_db "UPDATE hero_back_card SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE kfz_3v3_award SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE kfz_arena_award SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_league_audience")
	update_db "UPDATE kfz_league_audience SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE kfz_boss_reward SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE kfz_friend_gift SET id=id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_alliance_log")
	update_db "UPDATE kfz_alliance_log SET player_id1=player_id1+$RoleOffset, player_id2=player_id2+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_alliance_result")
	update_db "UPDATE kfz_alliance_result SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_alliance_result1")
	update_db "UPDATE kfz_alliance_result1 SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_league_result")
	update_db "UPDATE kfz_league_result SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_league_result1")
	update_db "UPDATE kfz_league_result1 SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_league_bet")
	update_db "UPDATE kfz_league_bet SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_quiz_result")
	update_db "UPDATE kfz_quiz_result SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_quiz_result1")
	update_db "UPDATE kfz_quiz_result1 SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_quiz_award")
	update_db "UPDATE kfz_quiz_award SET id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM kfz_star_result")
	update_db "UPDATE kfz_star_result SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(kfz_team_id) FROM kfz_team")
	update_db "UPDATE kfz_team SET kfz_team_leader=kfz_team_leader+$RoleOffset, kfz_team_id=kfz_team_id+$AutoID ORDER BY kfz_team_id DESC"
    update_db "UPDATE player_low SET kfz_team_id=kfz_team_id+$AutoID where kfz_team_id > 0 ORDER BY kfz_team_id DESC"
	update_db "UPDATE kfz_territory_reward SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_activity")
	update_db "UPDATE log_activity SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_anqi_mastery")
	update_db "UPDATE log_anqi_mastery SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_anqi_skill")
	update_db "UPDATE log_anqi_skill SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_anqi_upgrade")
	update_db "UPDATE log_anqi_upgrade SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_attribute_wash")
	update_db "UPDATE log_attribute_wash SET player_id=player_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_backout")
	update_db "UPDATE log_backout SET player_id=player_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_bless_box")
	update_db "UPDATE log_bless_box SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_box")
	update_db "UPDATE log_box SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_chengjiu")
	update_db "UPDATE log_chengjiu SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_comeback")
	update_db "UPDATE log_comeback SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_compose")
	update_db "UPDATE log_compose SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_consume_coin")
	update_db "UPDATE log_consume_coin SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_consume_gold")
	update_db "UPDATE log_consume_gold SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_consume_point")
	update_db "UPDATE log_consume_point SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_daily SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_darkgold_backout")
	update_db "UPDATE log_darkgold_backout SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_darkgold_bless")
	update_db "UPDATE log_darkgold_bless SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_darkgold_inlay")
	update_db "UPDATE log_darkgold_inlay SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_darkgold_purity")
	update_db "UPDATE log_darkgold_purity SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_darkgold_upgrade")
	update_db "UPDATE log_darkgold_upgrade SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_darkgold_wash")
	update_db "UPDATE log_darkgold_wash SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_couple_ring_open")
	update_db "UPDATE log_couple_ring_open SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_dungeon_entrust")
	update_db "UPDATE log_dungeon_entrust SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_equip_cl")
	update_db "UPDATE log_equip_cl SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_equip_cl SET gid=gid+$GoodsOffset where gid > 0 ORDER BY gid DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_equip_compose")
	update_db "UPDATE log_equip_compose SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_equip_forge")
	update_db "UPDATE log_equip_forge SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_equip_forge SET new_gid=new_gid+$GoodsOffset where new_gid > 0 ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_equip_refine")
	update_db "UPDATE log_equip_refine SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_equip_skill")
	update_db "UPDATE log_equip_skill SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_exchange")
	update_db "UPDATE log_exchange SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_expose")
	update_db "UPDATE log_expose SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_fashion_change")
	update_db "UPDATE log_fashion_change SET player_id=player_id+$RoleOffset, gid=gid+$GoodsOffset, cid=cid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_fashion_ticket")
	update_db "UPDATE log_fashion_ticket SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_fly_egg_strln")
	update_db "UPDATE log_fly_egg_strln SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_forge")
	update_db "UPDATE log_forge SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_fortune")
	update_db "UPDATE log_fortune SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_fortune SET refresh_role=refresh_role+$RoleOffset WHERE refresh_role > 0 ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_friend_delete")
	update_db "UPDATE log_friend_delete SET idA=idA+$RoleOffset, idB=idB+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_fxgz")
	update_db "UPDATE log_fxgz SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_gift")
	update_db "UPDATE log_gift SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_gift SET gid=gid+$GoodsOffset where gid > 0 ORDER BY gid DESC"
    AutoID=$(get_id "SELECT MAX(log_id) FROM log_gm_mail")
	update_db "UPDATE log_gm_mail SET id=id+$RoleOffset, log_id=log_id+$AutoID ORDER BY log_id DESC"
    update_db "UPDATE log_goods SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_goods_merge")
	update_db "UPDATE log_goods_merge SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_goods_reclaim")
	update_db "UPDATE log_goods_reclaim SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_goods_sell")
	update_db "UPDATE log_goods_sell SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_goods_sign")
	update_db "UPDATE log_goods_sign SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_goods_smelt")
	update_db "UPDATE log_goods_smelt SET role_id=role_id+$RoleOffset, gid1=gid1+$GoodsOffset, gid2=gid2+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_goods_use")
	update_db "UPDATE log_goods_use SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_group_buying")
	update_db "UPDATE log_group_buying SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_award")
	update_db "UPDATE log_guild_award SET guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_guild_award SET player_id=player_id+$RoleOffset where player_id > 0 ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_banquet_begin")
	update_db "UPDATE log_guild_banquet_begin SET player_id=player_id+$RoleOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_banquet_book")
	update_db "UPDATE log_guild_banquet_book SET player_id=player_id+$RoleOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_dial")
	update_db "UPDATE log_guild_dial SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_goods")
	update_db "UPDATE log_guild_goods SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_mall_exchange")
	update_db "UPDATE log_guild_mall_exchange SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_skill_active")
	update_db "UPDATE log_guild_skill_active SET player_id=player_id+$RoleOffset, guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_guild_skill_point")
	update_db "UPDATE log_guild_skill_point SET give_id=give_id+$RoleOffset, rec_id=rec_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_hanger")
	update_db "UPDATE log_hanger SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_happy_bag")
	update_db "UPDATE log_happy_bag SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_hole")
	update_db "UPDATE log_hole SET player_id=player_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_inlay")
	update_db "UPDATE log_inlay SET player_id=player_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_inlay SET sid=sid+$GoodsOffset WHERE sid > 0 ORDER BY id DESC"
    update_db "UPDATE log_jy SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_kfz_terr_setting")
	update_db "UPDATE log_kfz_terr_setting SET guild_id=guild_id+$GuildOffset, role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_kfz_terr_tender")
	update_db "UPDATE log_kfz_terr_tender SET guild_id=guild_id+$GuildOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_kfz_terr_reward")
	update_db "UPDATE log_kfz_terr_reward SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_login")
	update_db "UPDATE log_login SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_lucky_box")
	update_db "UPDATE log_lucky_box SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mail_info")
	update_db "UPDATE log_mail_info SET pid2=pid2+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_mail_info SET pid1=pid1+$RoleOffset where pid1 > 0 ORDER BY pid1 DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_material")
	update_db "UPDATE log_material SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_medal_soul")
	update_db "UPDATE log_medal_soul SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_meridian")
	update_db "UPDATE log_meridian SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_active")
	update_db "UPDATE log_mount_active SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_equip")
	update_db "UPDATE log_mount_equip SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_equip5x_back")
	update_db "UPDATE log_mount_equip5x_back SET player_id=player_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_equip5x_wash")
	update_db "UPDATE log_mount_equip5x_wash SET player_id=player_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_familiar")
	update_db "UPDATE log_mount_familiar SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_feed")
	update_db "UPDATE log_mount_feed SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_return")
	update_db "UPDATE log_mount_return SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_skill")
	update_db "UPDATE log_mount_skill SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_stren")
	update_db "UPDATE log_mount_stren SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_szbf")
	update_db "UPDATE log_mount_szbf SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_throw")
	update_db "UPDATE log_mount_throw SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_mount_upgrade")
	update_db "UPDATE log_mount_upgrade SET role_id=role_id+$RoleOffset, mount_id=mount_id+$MountOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_online_time")
	update_db "UPDATE log_online_time SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_ore_drop SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pay")
	update_db "UPDATE log_pay SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_personal_dial")
	update_db "UPDATE log_personal_dial SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet")
	update_db "UPDATE log_pet SET player_id=player_id+$RoleOffset, pet_id=pet_id+$PetOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet_aircraft_active")
	update_db "UPDATE log_pet_aircraft_active SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet_equip_active")
	update_db "UPDATE log_pet_equip_active SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet_equip_craft")
	update_db "UPDATE log_pet_equip_craft SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet_equip_exchange")
	update_db "UPDATE log_pet_equip_exchange SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet_equip_goods")
	update_db "UPDATE log_pet_equip_goods SET player_id=player_id+$RoleOffset, goods_id=goods_id+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet_equip_reclaim")
	update_db "UPDATE log_pet_equip_reclaim SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE log_pet_equip_reclaim SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_pet_equip_score")
	update_db "UPDATE log_pet_equip_score SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_phase_dungeon")
	update_db "UPDATE log_phase_dungeon SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_prefix_upgrade")
	update_db "UPDATE log_prefix_upgrade SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_produce_coin")
	update_db "UPDATE log_produce_coin SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_produce_gold")
	update_db "UPDATE log_produce_gold SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_rank_gift")
	update_db "UPDATE log_rank_gift SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_rank_top_hero_gift")
	update_db "UPDATE log_rank_top_hero_gift SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_rebate")
	update_db "UPDATE log_rebate SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_rename")
	update_db "UPDATE log_rename SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_resolve")
	update_db "UPDATE log_resolve SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset, new_id=new_id+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_secret_shop")
	update_db "UPDATE log_secret_shop SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_sell")
	update_db "UPDATE log_sell SET seller=seller+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_sell SET payer=payer+$RoleOffset where payer > 0 ORDER BY payer DESC"
    update_db "UPDATE log_sell SET gid=gid+$GoodsOffset where gid > 0 ORDER BY gid DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_soul_forge")
	update_db "UPDATE log_soul_forge SET player_id=player_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_soul_inpouring")
	update_db "UPDATE log_soul_inpouring SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_stone_compose")
	update_db "UPDATE log_stone_compose SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_stone_enchant")
	update_db "UPDATE log_stone_enchant SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_stren")
	update_db "UPDATE log_stren SET player_id=player_id+$RoleOffset, gid=gid+$GoodsOffset, sid=sid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_suit_resolve")
	update_db "UPDATE log_suit_resolve SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset, new_id=new_id+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_super_stren")
	update_db "UPDATE log_super_stren SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_taobao")
	update_db "UPDATE log_taobao SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_target_day")
	update_db "UPDATE log_target_day SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_target_week_task")
	update_db "UPDATE log_target_week_task SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_target_week_reward")
	update_db "UPDATE log_target_week_reward SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_task_cumulate")
	update_db "UPDATE log_task_cumulate SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_td SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    update_db "UPDATE log_td1 SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_throw")
	update_db "UPDATE log_throw SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_throw SET gid=gid+$GoodsOffset where gid > 0 ORDER BY gid DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_top_forging")
	update_db "UPDATE log_top_forging SET role_id=role_id+$RoleOffset, gid=gid+$GoodsOffset, new_gid=new_gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_top_suit")
	update_db "UPDATE log_top_suit SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_tower")
	update_db "UPDATE log_tower SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_trade")
	update_db "UPDATE log_trade SET pid1=pid1+$RoleOffset, pid2=pid2+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_treasure_hunt")
	update_db "UPDATE log_treasure_hunt SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_uplv")
	update_db "UPDATE log_uplv SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_vip_privilege")
	update_db "UPDATE log_vip_privilege SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_vip_privilege_reward")
	update_db "UPDATE log_vip_privilege_reward SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_wage")
	update_db "UPDATE log_wage SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_warrior")
	update_db "UPDATE log_warrior SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_warrior_attribute")
	update_db "UPDATE log_warrior_attribute SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM log_weapon_compose")
	update_db "UPDATE log_weapon_compose SET pid=pid+$RoleOffset, gid=gid+$GoodsOffset, new_gid=new_gid+$GoodsOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_del")
	update_db "UPDATE log_wing_del SET role_id=role_id+$RoleOffset, wing_id=wing_id+$WingOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_dungeon_daily_reward")
	update_db "UPDATE log_wing_dungeon_daily_reward SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_dungeon_reward")
	update_db "UPDATE log_wing_dungeon_reward SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_exp")
	update_db "UPDATE log_wing_exp SET role_id=role_id+$RoleOffset, wing_id=wing_id+$WingOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_fly")
	update_db "UPDATE log_wing_fly SET role_id=role_id+$RoleOffset, wing_id=wing_id+$WingOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_huanhua")
	update_db "UPDATE log_wing_huanhua SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_skill")
	update_db "UPDATE log_wing_skill SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_skill_map")
	update_db "UPDATE log_wing_skill_map SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_stren")
	update_db "UPDATE log_wing_stren SET role_id=role_id+$RoleOffset, wing_id=wing_id+$WingOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_train")
	update_db "UPDATE log_wing_train SET role_id=role_id+$RoleOffset, wing_id=wing_id+$WingOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wing_practice")
	update_db "UPDATE log_wing_practice SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM log_wtb")
	update_db "UPDATE log_wtb SET payer=payer+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE log_wtb SET seller=seller+$RoleOffset where seller > 0 ORDER BY seller DESC"
    update_db "UPDATE log_wtb SET gid=gid+$GoodsOffset where gid > 0 ORDER BY gid DESC"
    update_db "UPDATE lottry_lucker SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    update_db "UPDATE lottry_tick SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM lucky_animal")
	update_db "UPDATE lucky_animal SET id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE lucky_animal_role SET id=id+$RoleOffset, lucky_id=lucky_id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM mail_activity")
	update_db "UPDATE mail_activity SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE mail_queue SET activity_id=activity_id+$AutoID ORDER BY activity_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM mail_queue")
	update_db "UPDATE mail_queue SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM mail_attr")
	update_db "UPDATE mail_attr SET sid=sid+$RoleOffset, uid=uid+$RoleOffset,id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE mail_attr SET goods_id=goods_id+$GoodsOffset where id_type = 0 and goods_id > 0 ORDER BY id DESC"
    update_db "UPDATE mail_attr SET sid=0 WHERE TYPE=1 AND sid<>0"
	update_db "UPDATE log_sell SET mail=mail+$AutoID where mail > 0 ORDER BY mail DESC"
    update_db "UPDATE log_wtb SET mail=mail+$AutoID where mail > 0 ORDER BY mail DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM mail_content")
	update_db "UPDATE mail_content SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM marriage_event")
	update_db "UPDATE marriage_event SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE master SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE master_apprentice SET id=id+$RoleOffset, master_id=master_id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM meridian")
	update_db "UPDATE meridian SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE meridian SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE money_tree SET id=id+$RoleOffset, guild_id=guild_id+$GuildOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM money_tree_log")
	update_db "UPDATE money_tree_log SET role_id=role_id+$RoleOffset, tree_owner=tree_owner+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE mount SET role_id=role_id+$RoleOffset, id=id+$MountOffset ORDER BY id DESC"
	update_db "UPDATE mount SET equip1=equip1+$GoodsOffset where equip1 > 0 ORDER BY equip1 DESC"
    update_db "UPDATE mount SET equip2=equip2+$GoodsOffset where equip2 > 0 ORDER BY equip2 DESC"
    update_db "UPDATE mount_equip SET mount_id=mount_id+$MountOffset ORDER BY mount_id DESC"
    update_db "UPDATE mount_equip5x SET mount_id=mount_id+$MountOffset, player_id=player_id+$RoleOffset ORDER BY mount_id DESC"
    update_db "UPDATE mount_skill SET mount_id=mount_id+$MountOffset ORDER BY mount_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM mount_equip")
	update_db "UPDATE mount_equip SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM mount_skill")
	update_db "UPDATE mount_skill SET role_id=role_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE log_mount_skill SET skill_id=skill_id+$AutoID where skill_id > 0 ORDER BY skill_id DESC"
    update_db "UPDATE mount_szbf SET gid=gid+$GoodsOffset ORDER BY gid DESC"
    update_db "UPDATE mount_szbf SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    update_db "UPDATE new_ser_gift SET id=id+$RoleOffset ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM offline_msg")
	update_db "UPDATE offline_msg SET role_id=role_id+$RoleOffset, sender_id=sender_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet")
	update_db "UPDATE pet SET player_id=player_id+$RoleOffset, id=id+$PetOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet_aircraft")
	update_db "UPDATE pet_aircraft SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE pet_aircraft SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet_equip")
	update_db "UPDATE pet_equip SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE pet_equip SET equiped=equiped+$PetOffset WHERE equiped > 0 ORDER BY equiped DESC"
	update_db "UPDATE pet_equip_achieve SET pid=pid+$RoleOffset ORDER BY pid DESC"
	update_db "UPDATE pet_equip_status SET player_id=player_id+$RoleOffset, pet_id=pet_id+$PetOffset ORDER BY pet_id DESC"
	update_db "UPDATE pet_equip_status SET equip1=equip1+$AutoID WHERE equip1 > 0 ORDER BY equip1 DESC"
	update_db "UPDATE pet_equip_status SET equip2=equip2+$AutoID WHERE equip2 > 0 ORDER BY equip2 DESC"
	update_db "UPDATE pet_equip_status SET equip3=equip3+$AutoID WHERE equip3 > 0 ORDER BY equip3 DESC"
	update_db "UPDATE pet_equip_status SET equip4=equip4+$AutoID WHERE equip4 > 0 ORDER BY equip4 DESC"
	update_db "UPDATE pet_equip_status SET equip5=equip5+$AutoID WHERE equip5 > 0 ORDER BY equip5 DESC"
	update_db "UPDATE pet_equip_status SET equip6=equip6+$AutoID WHERE equip6 > 0 ORDER BY equip6 DESC"
	update_db "UPDATE pet_equip_status SET equip7=equip7+$AutoID WHERE equip7 > 0 ORDER BY equip7 DESC"
	update_db "UPDATE pet_equip_status SET equip8=equip8+$AutoID WHERE equip8 > 0 ORDER BY equip8 DESC"
	update_db "UPDATE pet_equip_status SET equip9=equip9+$AutoID WHERE equip9 > 0 ORDER BY equip9 DESC"
	update_db "UPDATE pet_equip_status SET equip10=equip10+$AutoID WHERE equip10 > 0 ORDER BY equip10 DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet_equip_box")
	update_db "UPDATE pet_equip_box SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE log_pet_equip_active SET equip_id=equip_id+$AutoID WHERE equip_id > 0 ORDER BY equip_id DESC"
	update_db "UPDATE log_pet_equip_craft SET equip_id1=equip_id1+$AutoID WHERE equip_id1 > 0 ORDER BY equip_id1 DESC"
	update_db "UPDATE log_pet_equip_craft SET equip_id2=equip_id2+$AutoID WHERE equip_id2 > 0 ORDER BY equip_id2 DESC"
	update_db "UPDATE log_pet_equip_exchange SET equip_id=equip_id+$AutoID WHERE equip_id > 0 ORDER BY equip_id DESC"
	update_db "UPDATE log_pet_equip_goods SET equip_id=equip_id+$AutoID WHERE equip_id > 0 ORDER BY equip_id DESC"
	update_db "UPDATE log_pet_equip_reclaim SET equip_id=equip_id+$AutoID WHERE equip_id > 0 ORDER BY equip_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet_manual")
	update_db "UPDATE pet_manual SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet_potential")
	update_db "UPDATE pet_potential SET player_id=player_id+$RoleOffset, pet_id=pet_id+$PetOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE pet_shop SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet_skill")
	update_db "UPDATE pet_skill SET pet_id=pet_id+$PetOffset, player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE pet_skill_treasurebox SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM pet_treasurebox")
	update_db "UPDATE pet_treasurebox SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE phase_dungeon SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE phase_dungeon_auto SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE player_attr SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE player_high SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE player_login SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE player_low SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE player_state SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE player_talent SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE practice SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM practice_outline")
	update_db "UPDATE practice_outline SET rold_id=rold_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM present_flower")
	update_db "UPDATE present_flower SET role_a=role_a+$RoleOffset, role_b=role_b+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM rank_history")
	update_db "UPDATE rank_history SET id=id+$AutoID ORDER BY id DESC"
    update_db "UPDATE rank_hurt_of_boss SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE rank_intraday_charge SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE rank_intraday_cost SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE rank_role_popularity SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE rank_wing_dungeon SET id=id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM relationship")
	update_db "UPDATE relationship SET idA=idA+$RoleOffset, idB=idB+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE rela_group SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE role_achieved_name SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM role_activity")
	update_db "UPDATE role_activity SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE role_activity SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE role_anqi SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE role_bless_box SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE role_fashion_figure SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE role_funds SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE role_funds_new SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE role_group_buying SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE role_happy_bag SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM role_hp_bag")
	update_db "UPDATE role_hp_bag SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE role_hp_bag SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM role_olympiad")
	update_db "UPDATE role_olympiad SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE role_olympiad SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE secondary_password SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE secret_shop SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM sell_list")
	update_db "UPDATE sell_list SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE sell_list SET gid=gid+$GoodsOffset where gid > 0 ORDER BY gid DESC"
	update_db "UPDATE log_sell SET sell_id=sell_id+$AutoID WHERE sell_id > 0 ORDER BY id DESC"
    update_db "UPDATE skill SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE skill2 SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE soul_inpouring SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE st_pay_user SET pid=pid+$RoleOffset ORDER BY pid DESC"
	#AutoID=$(get_id "SELECT MAX(id) FROM sworn_friends")
	update_db "UPDATE sworn_friends SET id=id+$SwornFriendsOffset ORDER BY id DESC"
	update_db "UPDATE sworn_member SET sworn_id=sworn_id+$SwornFriendsOffset, id=id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM taobao")
	update_db "UPDATE taobao SET id=id+$AutoID, role_id=role_id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE target_day SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE target_week SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE target_week_task SET player_id=player_id+$RoleOffset ORDER BY player_id DESC"
	update_db "UPDATE task_auto SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE task_bag SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE task_his SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE task_log SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE task_log_clear SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM treasure_hunt")
	update_db "UPDATE treasure_hunt SET id=id+$AutoID, role_id=role_id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE uplv_act_record SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE user_offline_status SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE vip_privilege SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE warrior SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE warrior_attribute SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE wenju_award SET pid=pid+$RoleOffset ORDER BY pid DESC"
	update_db "UPDATE wine_outline SET id=id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE wing SET id=id+$WingOffset, role_id=role_id+$RoleOffset ORDER BY id DESC"
	update_db "UPDATE wing_dungeon SET id=id+$RoleOffset ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM wing_fashion")
	update_db "UPDATE wing_fashion SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE wing_fashion SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	update_db "UPDATE wing_practice SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM wing_skill")
	update_db "UPDATE wing_skill SET id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE wing_skill SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM wishing_wall")
	update_db "UPDATE wishing_wall SET role_id=role_id+$RoleOffset, wishing_to=wishing_to+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM wtb_list")
	update_db "UPDATE wtb_list SET pid=pid+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	update_db "UPDATE log_wtb SET wtb_id=wtb_id+$AutoID WHERE wtb_id > 0 ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM couple_insure")
    update_db "UPDATE couple_insure SET id=id+$AutoID, husband=husband+$RoleOffset, wife=wife+$RoleOffset, applicant=applicant+$RoleOffset ORDER BY id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM couple_ring")
    update_db "UPDATE couple_ring SET id=id+$AutoID, role_id=role_id+$RoleOffset ORDER BY id DESC"
    update_db "UPDATE couple_ring SET goods_id=goods_id+$GoodsOffset WHERE goods_id>0 ORDER BY id DESC"
    update_db "UPDATE couple_ring_limit SET role_id=role_id+$RoleOffset ORDER BY role_id DESC"
    AutoID=$(get_id "SELECT MAX(id) FROM couple_sign")
    update_db "UPDATE couple_sign SET id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM wuju_log")
	update_db "UPDATE wuju_log SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM wuju_result")
	update_db "UPDATE wuju_result SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"
	AutoID=$(get_id "SELECT MAX(id) FROM wuju_match_log")
	update_db "UPDATE wuju_match_log SET player_id=player_id+$RoleOffset, id=id+$AutoID ORDER BY id DESC"

}

function update_db
{
	local query=${1}
	echo $query
	mysql -u$user -p$pass $src_db -S "${sock}" -e "$query"
}

function combine_sql
{
	echo "================ start combine database ================"
	for table in ${DBTables[@]}
	do
		Sql="INSERT INTO ${dst_db}.$table SELECT * FROM ${src_db}.$table"
		echo "$Sql"
		mysql -u$user -p$pass -S "${sock}" -e "$Sql"
	done
    ## 统计表相加
    Sql="UPDATE ${dst_db}.st_dairy s1 left join ${src_db}.st_dairy s2 on s1.time=s2.time set s1.reg_num=s1.reg_num+s2.reg_num, s1.login_num=s1.login_num+s2.login_num, s1.total_num=s1.total_num+s2.total_num, s1.pay_user=s1.pay_user+s2.pay_user, s1.pay_new=s1.pay_new+s2.pay_new, s1.pay_num=s1.pay_num+s2.pay_num, s1.pay_money=s1.pay_money+s2.pay_money, s1.pay_gold=s1.pay_gold+s2.pay_gold, s1.cost_gold=s1.cost_gold+s2.cost_gold, s1.remain_gold=s1.remain_gold+s2.remain_gold, s1.online_avg=s1.online_avg+s2.online_avg, s1.online_max=s1.online_max+s2.online_max, s1.vip_num=s1.vip_num+s2.vip_num, s1.remain_gold3=s1.remain_gold3+s2.remain_gold3, s1.remain_gold7=s1.remain_gold7+s2.remain_gold7, s1.rebate_gold=s1.rebate_gold+s2.rebate_gold, s1.old_ip_num=s1.old_ip_num+s2.old_ip_num where s2.time IS NOT NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"
    Sql="INSERT INTO ${dst_db}.st_dairy SELECT s2.* FROM ${src_db}.st_dairy s2 left join ${dst_db}.st_dairy s1 on s1.time=s2.time where s1.time IS NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"

    Sql="UPDATE ${dst_db}.st_consume_gold s1 left join ${src_db}.st_consume_gold s2 on s1.time=s2.time and s1.type=s2.type set s1.gold=s1.gold+s2.gold, s1.silver=s1.silver+s2.silver where s2.time IS NOT NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"
    Sql="INSERT INTO ${dst_db}.st_consume_gold SELECT s2.* FROM ${src_db}.st_consume_gold s2 left join ${dst_db}.st_consume_gold s1 on s1.time=s2.time and s1.type=s2.type where s1.time IS NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"

    Sql="UPDATE ${dst_db}.st_consume_coin s1 left join ${src_db}.st_consume_coin s2 on s1.time=s2.time and s1.type=s2.type set s1.coin=s1.coin+s2.coin, s1.bcoin=s1.bcoin+s2.bcoin where s2.time IS NOT NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"
    Sql="INSERT INTO ${dst_db}.st_consume_coin SELECT s2.* FROM ${src_db}.st_consume_coin s2 left join ${dst_db}.st_consume_coin s1 on s1.time=s2.time and s1.type=s2.type where s1.time IS NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"

    Sql="UPDATE ${dst_db}.st_consume_goods s1 left join ${src_db}.st_consume_goods s2 on s1.time=s2.time and s1.goods_id=s2.goods_id set s1.today_gold=s1.today_gold+s2.today_gold, s1.total_gold=s1.total_gold+s2.total_gold, s1.today_num=s1.today_num+s2.today_num, s1.total_num=s1.total_num+s2.total_num, s1.limit_today_gold=s1.limit_today_gold+s2.limit_today_gold, s1.limit_total_gold=s1.limit_total_gold+s2.limit_total_gold, s1.limit_today_num=s1.limit_today_num+s2.limit_today_num, s1.limit_total_num=s1.limit_total_num+s2.limit_total_num where s2.time IS NOT NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"
    Sql="INSERT INTO ${dst_db}.st_consume_goods SELECT s2.* FROM ${src_db}.st_consume_goods s2 left join ${dst_db}.st_consume_goods s1 on s1.time=s2.time and s1.goods_id=s2.goods_id where s1.time IS NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"

    Sql="UPDATE ${dst_db}.st_retention s1 left join ${src_db}.st_retention s2 on s1.time=s2.time set s1.reg_num=s1.reg_num+s2.reg_num, s1.login1=s1.login1+s2.login1, s1.login2=s1.login2+s2.login2, s1.login3=s1.login3+s2.login3, s1.login4=s1.login4+s2.login4, s1.login5=s1.login5+s2.login5, s1.login6=s1.login6+s2.login6, s1.login7=s1.login7+s2.login7, s1.login14=s1.login14+s2.login14, s1.login30=s1.login30+s2.login30 where s2.time IS NOT NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"
    Sql="INSERT INTO ${dst_db}.st_retention SELECT s2.* FROM ${src_db}.st_retention s2 left join ${dst_db}.st_retention s1 on s1.time=s2.time where s1.time IS NULL "
    echo "$Sql"
    mysql -u$user -p$pass -S "${sock}" -e "$Sql"
}


function conflict_name
{
	php patch_conflict_name.php $user $pass $dst_db $sock
}

function php_clear_data
{
	php data_clear.php $user $pass $dst_db $sock
}

function clear_data
{
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE arena_result"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE ip_limit"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE server"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE tower_masters"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE daily_log"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE guild_event"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE log_guildboss"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE td_rank"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE lottry_leiji"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE flag_battle_buff"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE flag_battle_mon_kill"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE flag_battle_win_guild"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE kfz_3v3_result"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE kfz_arena_result"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE coin_dungeon_rank"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE phase_dungeon_master"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE log_carnival"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE kfz_boss_guild_result"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE kfz_boss_result"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE log_kfz_boss_reward"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE log_kfz_boss_center"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE log_kfz_arena_award"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE sixiang_result"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "TRUNCATE TABLE guest"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "DELETE FROM role_achieved_name WHERE name_id>200000 AND name_id<300000 OR name_id=160001;"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE guild SET siege_time = 0,siege_result=0,siege_group = 0,siege_margin_flag = 0,siege_lasttime=0,siege_lastgroup=0,siege_base_scene_id=0,siege_forces_use=0,siege_stone_hurt=0,siege_end_flag=0,siege_occupier_image='[]',siege_tax_time=0,siege_occupy_num=0;"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE player_state SET kill_list='[]'"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "delete player_login from player_login left join player_low on player_login.id=player_low.id where player_low.id IS NULL"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "DELETE FROM task_his WHERE type=7"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "ALTER TABLE player_low DROP INDEX nickname , ADD UNIQUE nickname (nickname)"
	mysql -u$user -p$pass $dst_db -S "${sock}" -e "ALTER TABLE guild DROP INDEX index_guild_name , ADD UNIQUE index_guild_name (name) "
    ## 开启合服活动
    mysql -u$user -p$pass $dst_db -S "${sock}" -e "insert into base_time_control2 set time_start=unix_timestamp(curdate()), time_end=(unix_timestamp(curdate())+7*86400-1), type=55, list='[]' "
    mysql -u$user -p$pass $dst_db -S "${sock}" -e "insert into base_time_control2 set time_start=unix_timestamp(curdate()), time_end=(unix_timestamp(curdate())+3*86400-1), type=114, list='[]' "
    mysql -u$user -p$pass $dst_db -S "${sock}" -e "insert into base_time_control2 set ratio=2.0,time_start=(unix_timestamp(curdate())+86400+14*3600), time_end=(unix_timestamp(curdate())+86400+18*3600), type=56, list='[]' "
    mysql -u$user -p$pass $dst_db -S "${sock}" -e "insert into base_time_control2 set ratio=2.0,time_start=(unix_timestamp(curdate())+2*86400+14*3600), time_end=(unix_timestamp(curdate())+2*86400+18*3600), type=56, list='[]' "
    mysql -u$user -p$pass $dst_db -S "${sock}" -e "insert into base_time_control2 set ratio=2.0,time_start=(unix_timestamp(curdate())+3*86400+14*3600), time_end=(unix_timestamp(curdate())+3*86400+18*3600), type=56, list='[]' "
}

# PPTV的1服改账号名称，只是PPTV的1服需要这样做
#function pptv_mod_accname
#{
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE player_login SET accname = CONCAT('pptv_',accname) WHERE accname NOT LIKE 'pptv_%' and accname NOT LIKE 'xman_%' and accname <> ''; "
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE charge SET accname = CONCAT('pptv_',accname) WHERE accname NOT LIKE 'pptv_%' and accname NOT LIKE 'xman_%' and accname <> '';  "
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE base_gift_card SET accname = CONCAT('pptv_',accname) WHERE accname NOT LIKE 'pptv_%' and accname NOT LIKE 'xman_%' and accname <> '';  "
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE log_gm_mail SET accname = CONCAT('pptv_',accname) WHERE accname NOT LIKE 'pptv_%' and accname NOT LIKE 'xman_%' and accname <> '';  "
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE log_pay SET accname = CONCAT('pptv_',accname) WHERE accname NOT LIKE 'pptv_%' and accname NOT LIKE 'xman_%' and accname <> '';  "
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE log_sell SET accname1 = CONCAT('pptv_',accname1) WHERE accname1 NOT LIKE 'pptv_%' and accname1 NOT LIKE 'xman_%' and accname1 <> '';  "
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE log_sell SET accname2 = CONCAT('pptv_',accname2) WHERE accname2 NOT LIKE 'pptv_%' and accname2 NOT LIKE 'xman_%' and accname2 <> '';  "
#	mysql -u$user -p$pass $dst_db -S "${sock}" -e "UPDATE sell_list SET accname = CONCAT('pptv_',accname) WHERE accname NOT LIKE 'pptv_%' and accname NOT LIKE 'xman_%' and accname <> '';  "
#}

fix_db
combine_sql
conflict_name
clear_data
php_clear_data


