document.addEventListener("DOMContentLoaded", function () {
    const data = JSON.parse(document.getElementById('scores-chart').textContent);
    const labels = data.map(item => item.score);
    const votes = data.map(item => item.votes);
    const percentages = data.map(item => item.percentage);

    // const numberFormatter = new Intl.NumberFormat('en-UK'); // Formatter for numbers with commas

    const ctx = document.getElementById('scoresChart').getContext('2d');
    new Chart(ctx, {
        type: 'bar',
        data: {
            labels: labels,
            datasets: [
                {
                    label: 'Votes',
                    data: votes,
                    backgroundColor: 'rgba(75, 192, 192, 0.5)',
                    borderColor: 'rgba(75, 192, 192, 1)',
                    borderWidth: 1
                }
            ]
        },
        options: {
            indexAxis: 'y', // Switch the chart to horizontal orientation
            plugins: {
                title: {
                    display: true,
                    text: 'Score Breakdown',
                    font: {
                        size: 16,
                        weight: 'bold'
                    },
                    color: '#2c3e50',
                    padding: {
                        top: 20,
                        bottom: 20
                    }
                },
                legend: {
                    display: false
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            const index = context.dataIndex;
                            const numVotes = votes[index]; // context.dataset.data[index]
                            const percentage = percentages[index];
                            return `${numVotes.toLocaleString()} votes (${percentage}%)`;
                        }
                    }
                }
            },
            scales: {
                x: {
                    beginAtZero: true,
                    title: {
                        display: true,
                        text: 'Votes from Users',
                        font: {
                            size: 14
                        }
                    }
                },
                y: {
                    reverse: true,
                    title: {
                        display: true,
                        text: 'Score',
                        font: {
                            size: 14
                        }
                    }
                }
            }
        }
    });
});
