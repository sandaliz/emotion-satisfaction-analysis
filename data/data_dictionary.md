# Data Dictionary - Music Dataset (Sandali)

## Dataset: Emotify - Induced Musical Emotion
**Source**: Aljanaki et al., 2014 (Technical Report UU-CS-2014-015)
**Unit of Analysis**: Each row = one listening event (person's reaction to one song)

| Variable | Type | Description | Values/Range |
|----------|------|-------------|--------------|
| `track.id` | integer | Unique song identifier | 1-400 |
| `genre` | factor | Music genre (from folder structure) | classical, rock, electronic, pop |
| `amazement` | binary | Felt amazement? | 0 = No, 1 = Yes |
| `solemnity` | binary | Felt solemnity? | 0 = No, 1 = Yes |
| `tenderness` | binary | Felt tenderness? | 0 = No, 1 = Yes |
| `nostalgia` | binary | Felt nostalgia? | 0 = No, 1 = Yes |
| `calmness` | binary | Felt calmness? | 0 = No, 1 = Yes |
| `power` | binary | Felt power? | 0 = No, 1 = Yes |
| `joyful_activation` | binary | Felt joy/activation? | 0 = No, 1 = Yes |
| `tension` | binary | Felt tension? | 0 = No, 1 = Yes |
| `sadness` | binary | Felt sadness? | 0 = No, 1 = Yes |
| `mood` | integer | Participant's mood before game | 1 (very bad) to 5 (very good) |
| `liked` | binary | **SATISFACTION MEASURE** - Liked the song? | 0 = Disliked, 1 = Liked |
| `disliked` | binary | Disliked the song? | 0 = No, 1 = Yes |
| `age` | integer | Participant age | 10-82 (after cleaning) |
| `gender` | integer | **EMOTIONAL CONNECTION** - Gender code | 0 = Male, 1 = Female |
| `mother.tongue` | character | Participant's first language | English, Dutch, Spanish, etc. |

## Derived Variables (Created During Analysis)
| Variable | Type | Description | Calculation |
|----------|------|-------------|-------------|
| `emotional_intensity` | integer | **EMOTIONAL CONNECTION MEASURE** | Sum of 9 emotion columns (0-9) |
| `gender_label` | factor | Readable gender labels | 0→"Male", 1→"Female" |
| `age_group` | factor | Age categories | 0-12,13-18,19-25,26-35,36-50,50+ |
| `emo_quartile` | integer | Emotional intensity quartile | 1=Low, 2=Med-Low, 3=Med-High, 4=High |
| `emo_quartile_label` | factor | Labeled quartiles | Low, Med-Low, Med-High, High |