import { Component, computed, inject } from '@angular/core';
import { UserService } from '../../core';
import { CourseCardComponent } from '../../shared/course-card/course-card.component';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [CourseCardComponent],
  templateUrl: './dashboard.component.html',
  styleUrl: './dashboard.component.scss',
})
export class DashboardComponent {
  public user = inject(UserService);
  public username = computed(() => {
    return this.user.user().ime + ' ' + this.user.user().prezime;
  });

  public courses = computed(() => {
    return this.user.user().kolegiji;
  });

  constructor() {}
}
