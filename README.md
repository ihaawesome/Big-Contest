##  2019 빅콘테스트 퓨처스리그

> ## preprocess
### 1. train_preprocess_1.R
- **label**을 제외한 모든 데이터 셋에 대해 각각 계정 아이디(acc_id), 접속주차(week) 기준으로 전처리 후 acc_id로 조인해 변수 생성
- day 기준  activity와 combat 테이블의 일부 변수에서 극단치를 하위 1%, 상위 99% Quantile 값으로 대체
- 첫 접속주차(first_week)를 기준으로 1주차에 첫접속 유저  **Group 1**, 2~3주차 첫접속 유저  **Group 2**, 4주차 첫접속 유저 **Group 3**으로 그룹화
- 데이터 셋 'train', 'test1, 'test2' 구분 `set`을 입력하면 전처리 완료된 데이터 프레임 반환하는 함수 `make.data(set)`으로 정의, 각각 day, week 기준으로 전처리 완료된 *_week.csv, *_day.csv 파일을 저장함

산출물: train_week.csv, train_day.csv, test1_week.csv, test1_day.csv, test2_week.csv, test2_day.csv

### 2. train_preprocess_2.R
- raw 폴더 train_label  데이터의 `survival_time`을 기준으로 이탈여부 `churn` (이탈 1, 잔존 0), `amount_spent`을 기준으로 과금여부 `payment` (과금 1, 무과금 0) 를 나타내는 binary 변수 생성
	- preprocess 폴더에 다시 train_label.csv로 저장함
- 유저가 접속하지 않은 일자 역시 행으로 추가하고 값을 0으로 입력함
	- 총 40000*28=1120000개의 행을 가진 *_sequence 데이터 프레임을 생성하는 `make.sequence(set)`을 정의하고 각 *_day.csv 데이터 셋에 대해 시퀀스 배열을 위한 csv 파일을 저장

산출물: train_sequence.csv, test1_sequence.csv, test2_sequence.csv, train_label.csv


> ## model
### 1-1. churn_rnn_1_group*.ipynb
- 그룹별 이탈여부 예측 RNN 모형 (GRU 사용)
- F1-Score 기준으로 테스트 셋에서 스코어가 가장 높은 이탈 기준확률 값 찾음
	- 생존 모형에서 acc_id 하나당 64일 간 예측 생존확률 중에 이탈 기준확률보다 떨어지는 시점을 이탈시점으로 예측하는 데 사용

산출물: churn_rnn_1_group*.hdf5 (모델객체)

### 1-2. survival_model.R
- Cox-PH, Random Survival Forest 모형 적합 및 test 데이터에 대해 생존확률 예측
	- 그룹별 이탈 기준확률로 이탈시점 예측
	- 전체 acc_id 합해서 predict 폴더에 *_predict_survival.csv 저장

산출물: test1_predict_survival.csv, test2_predict_survival.csv

### 2. amount_spent_model.R
- XGBoost 모형 적합 및 test 데이터에 대해 평균 결제금액 예측
	- 전체 acc_id 합해서 predict 폴더에 *_predict_amount_spent.csv 저장

산출물: test1_predict_amount_spent.csv, test2_predict_amount_spent.csv


> ## predict
### predict_all.R
- *_predict_survival.csv, *_predict_amount_spent.csv 둘 다 불러와서 acc_id 별로 조인
	-  *_predict.csv로 저장
	
산출물: test1_predict.csv, test2_predict.csv
