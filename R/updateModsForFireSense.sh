#!/usr/bin/env bash
# updateModsForFireSense.sh -- bring the `modsForFireSense` integration branches up to date.
#
# Some PredictiveEcology PRs from the FireSense work stay open, unmerged, while other
# maintainers review them. The runs still need those changes, so each such repo carries a
# `modsForFireSense` branch = its base branch (development, or main where there is no
# development) + every open PR branch of ours, merged in with merge commits. Runs pin
# `<repo>@modsForFireSense`. Once a PR merges upstream its commits arrive with the base.
#
# History is never rewritten: an existing modsForFireSense branch is merged FORWARD (base
# first, then each PR branch) and pushed without force, because other people pin it too
# (e.g. LandR@modsForFireSense). A missing branch is created from the base. A push that is
# not a fast-forward is rejected by GitHub, which is the intended outcome.
#
# Conflicts resolved mechanically (see resolve_conflicts): NEWS.md keeps both sides;
# DESCRIPTION Version:/Date: and a module's `version = list(...)` line keep the base's (then
# max_version raises a package's Version to the highest merged); a
# one-line `reqdPkgs = list(...)` gets the union. Any other conflict aborts that one merge,
# the PR is reported, and the remaining PRs are still tried.
#
# Never touches anyone's working checkout: each repo is worked on in a detached worktree
# under $MODS_CACHE/worktrees/. The git store is ~/GitHub/<repo> when that clone has a
# remote pointing at PredictiveEcology/<repo>, otherwise $MODS_CACHE/<repo> (cloned once).
#
# Usage:
#   R/updateModsForFireSense.sh [--dry-run] [repo ...]     # no repo = every repo in CONFIG
# Env:
#   MODS_CACHE            default ~/GitHub/.modsForFireSense
#   MODS_COMMIT_TRAILERS  extra lines appended to each merge commit message (e.g. Co-Authored-By:)
# --dry-run does everything except the push; the merged result is left in the worktree.

set -uo pipefail

ORG=PredictiveEcology
BRANCH=modsForFireSense
CACHE=${MODS_CACHE:-$HOME/GitHub/.modsForFireSense}
TRAILERS=${MODS_COMMIT_TRAILERS:-}

# repo                    base         exclude (PR numbers)  order (listed first, rest by number)
# Automatic exclusions: head branch release/*, title containing "DO NOT MERGE" or
# "diagnostic", and a PR whose base is neither the repo's base nor modsForFireSense.
# Excluded by hand:
#   burnSummaries#12 (against modsForFireSense) is the reqdPkgs change of #10 again, older;
#     with #10 merged it only conflicts on the reqdPkgs line.
CONFIG='
LandR                     development  -                     248,250,251,255,256
climateData               development  -                     -
climateYear               development  -                     -
Biomass_borealDataPrep    development  -                     -
Biomass_core              development  -                     112,121
Biomass_summary           development  -                     -
NRV_summary               development  -                     -
burnSummaries             development  12                    -
CBM_core                  development  -                     -
CBM_dataPrep              development  -                     -
LandRCBM_split3pools      main         -                     -
fireSense_summary         development  -                     -
'

DRYRUN=0
SELECT=()
for a in "$@"; do
  case "$a" in
    --dry-run) DRYRUN=1 ;;
    -h|--help) sed -n '2,30p' "$0"; exit 0 ;;
    *) SELECT+=("$a") ;;
  esac
done

mkdir -p "$CACHE/worktrees"
SUMMARY=()

log() { printf '  %s\n' "$*"; }

# Locate (or create) the git store for a repo; echoes "<gitdir> <remote>".
git_store() {
  local repo=$1 dir=$HOME/GitHub/$repo r url
  if git -C "$dir" rev-parse --git-dir >/dev/null 2>&1; then
    for r in $(git -C "$dir" remote); do
      url=$(git -C "$dir" remote get-url "$r")
      if [[ $url =~ [/:]$ORG/$repo(\.git)?/?$ ]]; then echo "$dir $r"; return 0; fi
    done
  fi
  dir=$CACHE/$repo
  if [[ ! -d $dir ]]; then
    git clone -q --no-checkout "https://github.com/$ORG/$repo.git" "$dir" >&2 || return 1
  fi
  echo "$dir origin"
}

# Resolve the conflicts that are mechanical; returns 1 if any file needs a human.
#   NEWS.md                          keep both sides (ours first)
#   DESCRIPTION Version:/Date: lines keep the `keep` side (the base's)
#   module `version = list(x = "")`  keep the `keep` side
#   one-line `reqdPkgs = list(...)`  union of both; the same package with two specs is a conflict
RESOLVER='
function pkgname(t) { t = substr(t, 2, length(t) - 2); sub(/^.*\//, "", t); sub(/[ @(].*$/, "", t); return t }
function allver(A, n,   i) { if (n == 0) return 0; for (i = 1; i <= n; i++) if (A[i] !~ VER) return 0; return 1 }
function isreq(x) { return x ~ /^[ \t]*reqdPkgs = list\(.*\),?[ \t]*$/ }
function flush(   i, k, s, tok, nm, out, pre, post, n) {
  if (news) { for (i = 1; i <= no; i++) print O[i]; for (i = 1; i <= nt; i++) print T[i]; return }
  if (allver(O, no) && allver(T, nt)) {
    if (keep == "ours") for (i = 1; i <= no; i++) print O[i]; else for (i = 1; i <= nt; i++) print T[i]
    return
  }
  if (no == 1 && nt == 1 && isreq(O[1]) && isreq(T[1])) {
    pre = O[1]; sub(/list\(.*$/, "list(", pre); post = O[1]; sub(/^.*\)/, ")", post)
    delete seen; out = ""; n = 0
    for (k = 1; k <= 2; k++) {
      s = (k == 1) ? O[1] : T[1]
      while (match(s, /"[^"]*"/)) {
        tok = substr(s, RSTART, RLENGTH); s = substr(s, RSTART + RLENGTH); nm = pkgname(tok)
        if (nm in seen) { if (seen[nm] != tok) bad = 1; continue }
        seen[nm] = tok; out = out (n++ ? ", " : "") tok
      }
    }
    print pre out post; return
  }
  bad = 1
}
BEGIN { VER = "^((Version|Date):|[ \t]*version = list\\([A-Za-z0-9_.]+ = \"[^\"]*\"\\),?[ \t]*$)" }
/^<<<<<<< / { side = "o"; no = nt = 0; next }
/^=======$/ && side == "o" { side = "t"; next }
/^>>>>>>> / && side == "t" { flush(); side = ""; next }
side == "o" { O[++no] = $0; next }
side == "t" { T[++nt] = $0; next }
{ print }
END { exit bad }'

resolve_conflicts() {
  local keep=$1 f stages news
  for f in $(git diff --name-only --diff-filter=U); do
    stages=$(git ls-files -u -- "$f" | awk '{print $3}' | sort -u | tr -d '\n')
    if [[ $stages != *2*3* ]]; then log "conflict (add/delete) in $f"; return 1; fi
    news=0; [[ $(basename "$f") == NEWS.md ]] && news=1
    if ! awk -v keep="$keep" -v news="$news" "$RESOLVER" "$f" > "$f.tmp"; then
      rm -f "$f.tmp"; log "conflict in $f needs a human"; return 1
    fi
    mv "$f.tmp" "$f"
    git add -- "$f"
  done
  return 0
}

# merge_one <commit> <keep-for-DESCRIPTION> <subject> <body>; returns 0 merged/no-op, 1 conflict
merge_one() {
  local commit=$1 keep=$2 subject=$3 body=$4 msg
  if git merge-base --is-ancestor "$commit" HEAD; then return 0; fi
  msg=$(printf '%s\n\n%s\n' "$subject" "$body")
  [[ -n $TRAILERS ]] && msg=$(printf '%s\n\n%s\n' "$msg" "$TRAILERS")
  if git -c merge.conflictStyle=merge merge -q --no-ff --no-edit -m "$msg" "$commit" >/dev/null 2>&1; then
    return 0
  fi
  if resolve_conflicts "$keep" && git -c core.editor=true commit -q -m "$msg" >/dev/null; then
    log "resolved NEWS.md/DESCRIPTION conflicts mechanically"
    return 0
  fi
  git merge --abort 2>/dev/null
  return 1
}

parse_check() {
  local files
  files=$(git ls-files '*.R' '*.r' | tr '\n' ' ')
  [[ -z $files ]] && return 0
  # shellcheck disable=SC2086
  Rscript --vanilla -e '
    bad <- character()
    for (f in commandArgs(TRUE)) {
      r <- tryCatch({ parse(f, keep.source = FALSE); NULL }, error = function(e) conditionMessage(e))
      if (!is.null(r)) bad <- c(bad, paste0(f, ": ", r))
    }
    if (length(bad)) { cat(bad, sep = "\n"); quit(status = 1) }' $files
}

# A package's integration branch holds the base plus every merged PR, so its DESCRIPTION
# Version is the highest of theirs. Otherwise a module that floors on a PR's version (e.g.
# Biomass_borealDataPrep#132 needs LandR >= 1.2.0.9039 from LandR#248) is not satisfied by
# <pkg>@modsForFireSense and Require tries to replace it. Commits only if the version rises.
max_version() {
  [[ -f DESCRIPTION ]] || return 0
  local ref v cur top
  cur=$(sed -n 's/^Version: *//p' DESCRIPTION)
  top=$cur
  for ref in "$@"; do
    v=$(git show "$ref:DESCRIPTION" 2>/dev/null | sed -n 's/^Version: *//p')
    [[ -n $v ]] && top=$(printf '%s\n%s\n' "$top" "$v" | sort -V | tail -1)
  done
  [[ $top == "$cur" ]] && return 0
  sed -i "s/^Version: .*/Version: $top/" DESCRIPTION
  git commit -q -m "$BRANCH: Version $top (highest of the base and merged PR branches)" \
    ${TRAILERS:+-m "$TRAILERS"} -- DESCRIPTION
  log "Version $cur -> $top"
}

update_repo() {
  local repo=$1 base=$2 exclude=$3 order=$4
  local store gitdir remote wt start existed=0 merged=() skipped=() conflicts=() heads=() pushed="-"
  echo "== $repo (base $base)"
  store=$(git_store "$repo") || { SUMMARY+=("$repo|$base|-|clone failed|-"); return; }
  read -r gitdir remote <<<"$store"
  git -C "$gitdir" fetch -q --prune "$remote" || { SUMMARY+=("$repo|$base|-|fetch failed|-"); return; }

  if git -C "$gitdir" rev-parse -q --verify "refs/remotes/$remote/$BRANCH" >/dev/null; then
    existed=1; start=$remote/$BRANCH
  else
    start=$remote/$base
  fi
  wt=$CACHE/worktrees/$repo
  git -C "$gitdir" worktree remove --force "$wt" >/dev/null 2>&1
  rm -rf "$wt"; git -C "$gitdir" worktree prune
  git -C "$gitdir" worktree add -q --detach "$wt" "$start" || { SUMMARY+=("$repo|$base|-|worktree failed|-"); return; }
  cd "$wt" || return

  if (( existed )); then
    if ! merge_one "$remote/$base" theirs "Merge $base into $BRANCH" "Brings $BRANCH up to date with $base."; then
      conflicts+=("$base")
      log "CONFLICT merging $base forward; nothing else done for $repo"
      SUMMARY+=("$repo|$base|-|${conflicts[*]}|-"); cd - >/dev/null; return
    fi
  fi

  # Our open PRs, ordered: listed numbers first, then the rest ascending.
  local prs n head pbase title sha rank i o
  prs=$(gh pr list -R "$ORG/$repo" --author @me --state open --limit 100 \
          --json number,headRefName,baseRefName,title,headRefOid \
          --jq '.[] | [.number, .headRefName, .baseRefName, .headRefOid, .title] | @tsv')
  prs=$(while IFS=$'\t' read -r n head pbase sha title; do
          [[ -z $n ]] && continue
          rank=1000; i=0
          for o in ${order//,/ }; do i=$((i + 1)); [[ $o == "$n" ]] && rank=$i; done
          printf '%04d\t%05d\t%s\t%s\t%s\t%s\t%s\n' "$rank" "$n" "$n" "$head" "$pbase" "$sha" "$title"
        done <<<"$prs" | sort | cut -f3-)

  while IFS=$'\t' read -r n head pbase sha title; do
    [[ -z $n ]] && continue
    if [[ ",$exclude," == *",$n,"* ]]; then skipped+=("#$n(config)"); continue; fi
    if [[ $head == release/* ]]; then skipped+=("#$n(release)"); continue; fi
    if [[ ${title,,} == *"do not merge"* || ${title,,} == *diagnostic* ]]; then skipped+=("#$n(title)"); continue; fi
    if [[ $pbase != "$base" && $pbase != "$BRANCH" ]]; then skipped+=("#$n(base $pbase)"); continue; fi
    git fetch -q "$remote" "refs/pull/$n/head" || { conflicts+=("#$n(fetch)"); continue; }
    if git merge-base --is-ancestor "$sha" HEAD; then merged+=("#$n"); heads+=("$sha"); continue; fi
    if merge_one "$sha" ours "Merge PR #$n ($head) into $BRANCH" "$title"; then
      merged+=("#$n*"); heads+=("$sha"); log "merged #$n $head"
    else
      conflicts+=("#$n"); log "CONFLICT #$n $head -- skipped"
    fi
  done <<<"$prs"

  max_version "$remote/$base" "${heads[@]}"

  if ! parse_check; then
    conflicts+=("parse-check"); log "R parse check FAILED; not pushing"
  elif [[ $existed == 1 && $(git rev-parse HEAD) == $(git rev-parse "$remote/$BRANCH") ]]; then
    pushed="unchanged $(git rev-parse --short HEAD)"
  elif (( DRYRUN )); then
    pushed="dry-run $(git rev-parse --short HEAD)"
  elif git push -q "$remote" "HEAD:refs/heads/$BRANCH"; then
    pushed=$(git rev-parse --short HEAD)
  else
    pushed="PUSH FAILED"
  fi
  cd - >/dev/null || true
  (( DRYRUN )) || { git -C "$gitdir" worktree remove --force "$wt" >/dev/null 2>&1; git -C "$gitdir" worktree prune; }
  local m="${merged[*]:--}" c="${conflicts[*]:--}"
  [[ ${#skipped[@]} -gt 0 ]] && m="$m; excluded ${skipped[*]}"
  SUMMARY+=("$repo|$base|$m|$c|$pushed")
}

while read -r repo base exclude order; do
  [[ -z ${repo:-} || $repo == \#* ]] && continue
  if (( ${#SELECT[@]} )) && [[ " ${SELECT[*]} " != *" $repo "* ]]; then continue; fi
  update_repo "$repo" "$base" "$exclude" "$order"
done <<<"$CONFIG"

echo
echo "PRs: #n = already in $BRANCH, #n* = merged now"
{ echo "repo|base|PRs|conflicts|pushed"; printf '%s\n' "${SUMMARY[@]}"; } | column -t -s '|'
